// Copyright 2018 Commonwealth Labs, Inc.
// This file is part of Edgeware.

// Edgeware is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Edgeware is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Edgeware.  If not, see <http://www.gnu.org/licenses/>.

#[cfg(feature = "std")]
extern crate serde;

// Needed for deriving `Serialize` and `Deserialize` for various types.
// We only implement the serde traits for std builds - they're unneeded
// in the wasm runtime.
#[cfg(feature = "std")]
extern crate parity_codec as codec;
extern crate sr_io as runtime_io;
extern crate sr_primitives as runtime_primitives;
extern crate sr_std as rstd;
extern crate srml_support as runtime_support;
extern crate substrate_primitives as primitives;

extern crate srml_system as system;

use rstd::prelude::*;
use runtime_primitives::traits::{Hash, MaybeSerializeDebug};
use runtime_support::dispatch::Result;
use runtime_support::{Parameter, StorageMap, StorageValue};
use system::ensure_signed;

pub trait Trait: system::Trait {
	/// The claims type
	type Claim: Parameter + MaybeSerializeDebug;
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

pub type Attestation = Vec<u8>;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq)]
pub struct MetadataRecord {
	pub avatar: Vec<u8>,
	pub display_name: Vec<u8>,
	pub tagline: Vec<u8>,
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, Copy, Clone, Eq, PartialEq)]
pub enum IdentityStage {
	Registered,
	Attested,
	Verified,
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq)]
pub struct IdentityRecord<AccountId, BlockNumber> {
	pub account: AccountId,
	pub identity: Vec<u8>,
	pub stage: IdentityStage,
	pub expiration_time: Option<BlockNumber>,
	pub proof: Option<Attestation>,
	pub verifications: Option<Vec<(AccountId, bool)>>,
	pub metadata: Option<MetadataRecord>,
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event<T>() = default;

		/// Register an identity with the hash of the signature. Ensures that
		/// all identities are unique, so that no duplicate identities can be
		/// registered.
		///
		/// Current implementation suffers from squatter attacks. Additional
		/// implementations could provide a mechanism for a trusted set of
		/// authorities to delete a squatted identity OR implement storage
		/// rent to disincentivize it.
		pub fn register(origin, identity: Vec<u8>) -> Result {
			let _sender = ensure_signed(origin)?;
			ensure!(!Self::frozen_accounts().iter().any(|i| i == &_sender.clone()), "Sender account is frozen");

			let hash = T::Hashing::hash_of(&identity);
			ensure!(!<IdentityOf<T>>::exists(hash), "Identity already exists");

			let expiration = <system::Module<T>>::block_number() + Self::expiration_time();

			<Identities<T>>::mutate(|idents| idents.push(hash.clone()));
			<IdentityOf<T>>::insert(hash, IdentityRecord {
				account: _sender.clone(),
				identity: identity,
				stage: IdentityStage::Registered,
				expiration_time: Some(expiration),
				proof: None,
				verifications: None,
				metadata: None,
			});
			<IdentitiesPending<T>>::mutate(|idents| idents.push((hash, expiration)));

			Self::deposit_event(RawEvent::Register(hash, _sender.into()));
			Ok(())
		}

		/// Attest that the sender is the original publisher of said identity
		/// by linking to an external proof.
		///
		/// Current implementation overwrites all proofs if safety checks
		/// pass.
		pub fn attest(origin, identity_hash: T::Hash, attestation: Attestation) -> Result {
			let _sender = ensure_signed(origin)?;
			ensure!(!Self::frozen_accounts().iter().any(|i| i == &_sender.clone()), "Sender account is frozen");

			let record = <IdentityOf<T>>::get(&identity_hash).ok_or("Identity does not exist")?;

			if record.stage == IdentityStage::Verified {
				return Err("Already verified");
			}

			// Check that original sender and current sender match
			ensure!(record.account == _sender, "Stored identity does not match sender");

			let expiration = <system::Module<T>>::block_number() + Self::expiration_time();

			// TODO: Decide how we want to process proof updates
			// currently this implements no check against updating
			// proof links
			<IdentityOf<T>>::insert(identity_hash, IdentityRecord {
				proof: Some(attestation),
				stage: IdentityStage::Attested,
				expiration_time: Some(expiration),
				..record
			});

			<IdentitiesPending<T>>::mutate(|idents| {
				idents.retain(|(hash, _)| hash != &identity_hash);
				idents.push((identity_hash, expiration))
			});

			Self::deposit_event(RawEvent::Attest(identity_hash, _sender.into()));
			Ok(())
		}

		/// Verify an existing identity based on its attested proof. Sender
		/// be specified on the pre-selected list of verifiers.
		pub fn verify(origin, identity_hash: T::Hash, vote: bool) -> Result {
			let _sender = ensure_signed(origin)?;

			ensure!(Self::verifiers().contains(&_sender), "Sender not a verifier");
			let record = <IdentityOf<T>>::get(&identity_hash).ok_or("Identity does not exist")?;
			match record.stage {
				IdentityStage::Registered => return Err("No attestation to verify"),
				IdentityStage::Verified => return Err("Already verified"),
				IdentityStage::Attested => ()
			}

			// Check the number of verifications the record has and ensure all are valid
			let mut valid_verifications: Vec<(T::AccountId, bool)> = [(_sender.clone(), vote)].to_vec();
			let acct = record.account.clone();
			if let Some(verifications) = record.verifications {
				valid_verifications = verifications.clone()
					.into_iter()
					.filter(|v| Self::verifiers().contains(&v.0))
					.collect();
				valid_verifications.push((_sender.clone(), vote));
			}

			let yes_votes: Vec<T::AccountId> = valid_verifications.clone()
				.into_iter()
				.filter(|v| v.1 == true)
				.map(|v| v.0)
				.collect();

			let no_votes: Vec<T::AccountId> = valid_verifications.clone()
				.into_iter()
				.filter(|v| v.1 == false)
				.map(|v| v.0)
				.collect();

			// Check if we have gather a supermajority of no votes
			// TODO: What does it mean to fail a verification? If malicious
			//		 behavior, we should punish the sending account somehow.
			if no_votes.len() * 3 >= 2 * Self::verifiers().len() {
				Self::remove_pending_identity(&identity_hash, true);
				<FrozenAccounts<T>>::mutate(|froze| froze.push(acct));
				Self::deposit_event(RawEvent::Failed(identity_hash, record.account.into()));
			} else {
				// Check if we have gathered a supermajority of yes votes
				let mut stage = IdentityStage::Attested;
				let mut expiration_time = record.expiration_time;
				let registrar = record.account.clone();
				if yes_votes.len() * 3 >= 2 * Self::verifiers().len() {
					stage = IdentityStage::Verified;
					expiration_time = None;
				}
				// Insert updated identity record back into the collection
				<IdentityOf<T>>::insert(identity_hash, IdentityRecord {
					stage: stage,
					expiration_time: expiration_time,
					verifications: Some(valid_verifications),
					..record
				});

				if stage == IdentityStage::Verified {
					Self::remove_pending_identity(&identity_hash, false);
					Self::deposit_event(RawEvent::Verify(identity_hash, registrar.into(), yes_votes));
				}
			}

			Ok(())
		}

		/// Add metadata to sender's account.
		// TODO: make all options and only updated provided?
		// TODO: limit the max length of these user-submitted types?
		pub fn add_metadata(origin, identity_hash: T::Hash, avatar: Vec<u8>, display_name: Vec<u8>, tagline: Vec<u8>) -> Result {
			let _sender = ensure_signed(origin)?;
			let record = <IdentityOf<T>>::get(&identity_hash).ok_or("Identity does not exist")?;

			// Check that original sender and current sender match
			ensure!(record.account == _sender, "Stored identity does not match sender");

			// TODO: Decide how to process metadata updates, for now it's all or nothing
			let mut new_record = record;
			new_record.metadata = Some(MetadataRecord {
				avatar: avatar,
				display_name: display_name,
				tagline: tagline,
			});
			<IdentityOf<T>>::insert(identity_hash, new_record);
			// TODO: worth adding an event?
			Ok(())
		}

		/// Add a claim as a claims issuer. Ensures that the sender is currently
		/// an active claims issuer. Ensures that the identity exists by checking
		/// hash exists in the Identities map.
		pub fn add_claim(origin, identity_hash: T::Hash, claim: T::Claim) -> Result {
			let _sender = ensure_signed(origin)?;

			let issuers: Vec<T::AccountId> = Self::claims_issuers();
			ensure!(issuers.iter().any(|id| id == &_sender), "Invalid claims issuer");
			ensure!(<IdentityOf<T>>::exists(identity_hash), "Invalid identity record");

			let mut claims = Self::claims(identity_hash);
			claims.push((_sender.clone(), claim));
			<Claims<T>>::insert(identity_hash, claims);
			Ok(())
		}

		/// Remove a claim as a claims issuer. Ensures that the sender is an active
		/// claims issuer. Ensures that the sender has issued a claim over the
		/// identity provided to the module.
		pub fn remove_claim(origin, identity_hash: T::Hash) -> Result {
			let _sender = ensure_signed(origin)?;

			let issuers: Vec<T::AccountId> = Self::claims_issuers();
			ensure!(issuers.iter().any(|id| id == &_sender), "Invalid claims issuer");
			ensure!(<IdentityOf<T>>::exists(identity_hash), "Invalid identity record");

			let mut claims = Self::claims(identity_hash);
			ensure!(claims.iter().any(|claim| claim.0 == _sender.clone()), "No existing claim under issuer");

			let index = claims.iter().position(|claim| claim.0 == _sender.clone()).unwrap();
			claims.remove(index);
			<Claims<T>>::insert(identity_hash, claims);

			Ok(())
		}

		/// Check all pending identities for expiration when each block is
		/// finalised. Once an identity expires, it is deleted from storage.
		fn on_finalise(n: T::BlockNumber) {
			let (expired, valid): (Vec<_>, _) = <IdentitiesPending<T>>::get()
				.into_iter()
				.partition(|(_, expiration)| n >= *expiration);

			expired.into_iter().for_each(move |(exp_hash, _)| {
				<Identities<T>>::mutate(|idents| idents.retain(|hash| hash != &exp_hash));
				<IdentityOf<T>>::remove(exp_hash);
				Self::deposit_event(RawEvent::Expired(exp_hash))
			});
			<IdentitiesPending<T>>::put(valid);
		}
	}
}

impl<T: Trait> Module<T> {
	/// Removes all data about a pending identity given the hash of the record
	pub fn remove_pending_identity(identity_hash: &T::Hash, malicious: bool) {
		// If triggered by a malicious party's actions, delete all data
		if malicious {
			<Identities<T>>::mutate(|idents| idents.retain(|hash| hash != identity_hash));
			<IdentityOf<T>>::remove(identity_hash);			
		}

		<IdentitiesPending<T>>::mutate(|idents| {
			idents.retain(|(hash, _)| hash != identity_hash)
		});
	}
}

/// An event in this module.
decl_event!(
	pub enum Event<T> where <T as system::Trait>::Hash,
							<T as system::Trait>::AccountId,
							<T as Trait>::Claim {
		Register(Hash, AccountId),
		Attest(Hash, AccountId),
		Verify(Hash, AccountId, Vec<AccountId>),
		Failed(Hash, AccountId),
		Expired(Hash),
		AddedClaim(Hash, Claim, AccountId),
		RemovedClaim(Hash, Claim, AccountId),
	}
);

// TODO: rename "timeouts" "time limit" to ???
decl_storage! {
	trait Store for Module<T: Trait> as Identity {
		/// The hashed identities.
		pub Identities get(identities): Vec<(T::Hash)>;
		/// Actual identity for a given hash, if it's current.
		pub IdentityOf get(identity_of): map T::Hash => Option<IdentityRecord<T::AccountId, T::BlockNumber>>;
		/// List of identities awaiting attestation or verification and associated expirations
		pub IdentitiesPending get(identities_pending): Vec<(T::Hash, T::BlockNumber)>;
		/// List of malicious identities who submit failed attestations
		pub FrozenAccounts get(frozen_accounts): Vec<T::AccountId>;
		/// Number of blocks allowed between register/attest or attest/verify.
		pub ExpirationTime get(expiration_time) config(): T::BlockNumber;
		/// Accounts granted power to verify identities
		pub Verifiers get(verifiers) config(): Vec<T::AccountId>;
		/// The set of active claims issuers
		pub ClaimsIssuers get(claims_issuers) config(): Vec<T::AccountId>;
		/// The claims mapping for identity records: (claims_issuer, claim)
		pub Claims get(claims): map T::Hash => Vec<(T::AccountId, T::Claim)>;

	}
}
