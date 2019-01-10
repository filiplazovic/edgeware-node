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

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate serde;

// Needed for deriving `Serialize` and `Deserialize` for various types.
// We only implement the serde traits for std builds - they're unneeded
// in the wasm runtime.
#[cfg(feature = "std")]

extern crate parity_codec as codec;
extern crate substrate_primitives as primitives;
extern crate sr_std as rstd;
extern crate srml_support as runtime_support;
extern crate sr_primitives as runtime_primitives;
extern crate sr_io as runtime_io;

extern crate srml_balances as balances;
extern crate srml_system as system;

use rstd::prelude::*;
use system::ensure_signed;
use runtime_support::{StorageValue, StorageMap};
use runtime_support::dispatch::Result;

// TODO: move this into trait
type VoteId = u32;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq)]
pub struct VotingResults {
	pub yes: u32,
	pub no: u32,
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq)]
pub struct VotingRecord<AccountId> {
	pub id: VoteId,
	pub completed: bool,
	pub voted: Vec<AccountId>,
	pub delegated: Vec<AccountId>,
	pub results: Option<VotingResults>,
}

pub trait Trait: system::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;

		/// Submit a vote, overwriting old votes and delegations if found.
		pub fn vote(origin, id: VoteId, vote: bool) -> Result {
			let _sender = ensure_signed(origin)?;
			let mut record = <Votes<T>>::get(&id).ok_or("Vote does not exist")?;
			ensure!(!record.completed, "Vote already completed");
			Self::undelegate_if_exists(&mut record, &_sender);

			// add to voters if not already voted
			if !record.voted.contains(&_sender) {
				record.voted.push(_sender.clone());
				Self::deposit_event(RawEvent::Voted(id, _sender.clone(), vote));
			} else {
				// TODO: prevent updating to same position?
				Self::deposit_event(RawEvent::VoteUpdated(id, _sender.clone(), vote));
			}
			<VoteOf<T>>::insert((id, _sender), vote);
			<Votes<T>>::insert(id, record);
			Ok(())
		}

		pub fn unvote(origin, id: VoteId) -> Result {
			let _sender = ensure_signed(origin)?;
			let mut record = <Votes<T>>::get(&id).ok_or("Vote does not exist")?;
			ensure!(!record.completed, "Vote already completed");
			Self::unvote_if_exists(&mut record, &_sender);
			Ok(())
		}

		pub fn delegate(origin, id: VoteId, to: T::AccountId) -> Result {
			let _sender = ensure_signed(origin)?;
			let mut record = <Votes<T>>::get(&id).ok_or("Vote does not exist")?;
			ensure!(!record.completed, "Vote already completed");
			ensure!(!Self::has_delegation_cycle(id, &_sender, to.clone()), "Invalid delegation");
			Self::unvote_if_exists(&mut record, &_sender);

			// add to delegates if not already delegated
			if !record.delegated.contains(&_sender) {
				record.delegated.push(_sender.clone());
				Self::deposit_event(RawEvent::Delegated(id, _sender.clone(), to.clone()));
			} else {
				Self::deposit_event(RawEvent::DelegationUpdated(id, _sender.clone(), to.clone()));
			}
			<DelegateOf<T>>::insert((id, _sender), to);
			<Votes<T>>::insert(id, record);
			Ok(())
		}

		pub fn undelegate(origin, id: VoteId) -> Result {
			let _sender = ensure_signed(origin)?;
			let mut record = <Votes<T>>::get(&id).ok_or("Vote does not exist")?;
			ensure!(!record.completed, "Vote already completed");
			Self::undelegate_if_exists(&mut record, &_sender);
			Ok(())
		}
	}
}

impl<T: Trait> Module<T> {
	pub fn create_vote() -> VoteId {
		let id = <NumVotes<T>>::get();
		<NumVotes<T>>::mutate(|i| *i += 1);
		<Votes<T>>::insert(id, VotingRecord {
			id: id,
			completed: false,
			voted: vec![],
			delegated: vec![],
			results: None,
		});
		return id;
	}

	fn unvote_if_exists(vote: &mut VotingRecord<T::AccountId>, who: &T::AccountId) {
		if let Some(i) = &vote.voted.iter().position(|ref s| s == &who) {
			vote.voted.swap_remove(*i);
			<VoteOf<T>>::remove((vote.id, who.clone()));
			Self::deposit_event(RawEvent::Unvoted(vote.id, who.clone()));
		}
	}

	fn undelegate_if_exists(vote: &mut VotingRecord<T::AccountId>, who: &T::AccountId) {
		if let Some(i) = &vote.delegated.iter().position(|ref s| s == &who) {
			vote.delegated.swap_remove(*i);
			<DelegateOf<T>>::remove((vote.id, who.clone()));
			Self::deposit_event(RawEvent::Undelegated(vote.id, who.clone()));
		}
	}

	/// Implement rudimentary DFS to find if "to"'s delegation ever leads to "from"	
	fn has_delegation_cycle(id: VoteId, from: &T::AccountId, to: T::AccountId) -> bool {
		// Loop over delegation path of "to" to check if "from" exists
		if from == &to {
			return true;
		}
		match Self::delegate_of((id, to)) {
			Some(delegate) => Self::has_delegation_cycle(id, from, delegate),
			None => false,
		}
	}

	/// Get the last node at the end of a delegation path for a given account
	fn get_sink_delegator(id: VoteId, start: T::AccountId) -> T::AccountId {
		match Self::delegate_of((id, start.clone())) {
			Some(delegate) => Self::get_sink_delegator(id, delegate),
			None => start,
		}
	}

	pub fn complete_vote(id: VoteId) -> Result {
		let record = <Votes<T>>::get(&id).ok_or("Vote does not exist")?;
		ensure!(!record.completed, "Vote already completed");
		
		// accumulate results via fold, resolving delegations as we go
		let fold_fn = |r: VotingResults, v: &T::AccountId| {
			match Self::vote_of((id, Self::get_sink_delegator(id, v.clone()))) {
				Some(true) => VotingResults { yes: r.yes + 1, no: r.no },
				Some(false) => VotingResults { yes: r.yes, no: r.no + 1 },
				None => r,
			}
		};
		let zero_result = VotingResults { yes: 0, no: 0 };
		let delegate_results = record.delegated.iter().fold(zero_result, fold_fn);
		let results = record.voted.iter().fold(delegate_results, fold_fn);
		// TODO: clean up DelegateOf/VoteOf maps?
		<Votes<T>>::insert(id, VotingRecord {
			results: Some(results),
			..record
		});
		Self::deposit_event(RawEvent::VoteCompleted(id));
		Ok(())
	}
}

/// An event in this module.
decl_event!(
	pub enum Event<T> where <T as system::Trait>::AccountId {
		VoteCreated(VoteId),
		Voted(VoteId, AccountId, bool),
		VoteUpdated(VoteId, AccountId, bool),
		Unvoted(VoteId, AccountId),
		Delegated(VoteId, AccountId, AccountId),
		DelegationUpdated(VoteId, AccountId, AccountId),
		Undelegated(VoteId, AccountId),
		VoteCompleted(VoteId),
	}
);

decl_storage! {
	trait Store for Module<T: Trait> as Delegation {
		/// The number of votes held thus far.
		pub NumVotes get(num_votes): VoteId;
		pub Votes get(votes): map VoteId => Option<VotingRecord<T::AccountId>>;
		/// The map of strict delegates for each account
		pub DelegateOf get(delegate_of): map (VoteId, T::AccountId) => Option<T::AccountId>;
		pub VoteOf get(vote_of): map (VoteId, T::AccountId) => Option<bool>;
	}
}
