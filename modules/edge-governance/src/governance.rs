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
extern crate srml_system as system;
extern crate edge_delegation as delegation;

use rstd::prelude::*;
use system::ensure_signed;
use runtime_support::{StorageValue, StorageMap};
use runtime_support::dispatch::Result;
use runtime_primitives::traits::Hash;
use codec::Encode;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Clone, Copy)]
pub enum ProposalStage {
	PreVoting,
	Voting,
	Completed,
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Clone, Copy)]
pub enum ProposalCategory {
	Signaling,
	Funding,
	Upgrade,
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq)]
pub struct ProposalRecord<AccountId, BlockNumber> {
	pub index: u32,
	pub author: AccountId,
	pub stage: ProposalStage,
	pub transition_block: Option<BlockNumber>,
	pub category: ProposalCategory,
	pub title: Vec<u8>,
	pub contents: Vec<u8>,
	// TODO: separate comments into different object, for storage reasons
	pub comments: Vec<(Vec<u8>, AccountId)>,
	// TODO: for actions, we might need more data
}

pub trait Trait: delegation::Trait {
	/// The overarching event type
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;

		/// Creates a new governance proposal in the chosen category.
		pub fn create_proposal(origin, title: Vec<u8>, contents: Vec<u8>, category: ProposalCategory) -> Result {
			let _sender = ensure_signed(origin)?;
			ensure!(!title.is_empty(), "Proposal must have title");
			ensure!(!contents.is_empty(), "Proposal must not be empty");

			// construct hash(origin + proposal) and check existence
			// TODO: include title/category/etc?
			let mut buf = Vec::new();
			buf.extend_from_slice(&_sender.encode());
			buf.extend_from_slice(&contents.as_ref());
			let hash = T::Hashing::hash(&buf[..]);
			ensure!(<ProposalOf<T>>::get(hash) == None, "Proposal already exists");

			let index = <ProposalCount<T>>::get();
			<ProposalCount<T>>::mutate(|i| *i += 1);
			<ProposalOf<T>>::insert(hash, ProposalRecord {
				index: index,
				author: _sender.clone(),
				stage: ProposalStage::PreVoting,
				category: category,
				transition_block: None,
				title: title,
				contents: contents,
				comments: vec![]
			});
			<Proposals<T>>::mutate(|proposals| proposals.push(hash));
			Self::deposit_event(RawEvent::NewProposal(_sender, hash));
			Ok(())
		}

		/// Add a new comment to an existing governance proposal.
		// TODO: give comments unique numbers/ids?
		pub fn add_comment(origin, proposal_hash: T::Hash, comment: Vec<u8>) -> Result {
			let _sender = ensure_signed(origin)?;
			let record = <ProposalOf<T>>::get(proposal_hash).ok_or("Proposal does not exist")?;
			// TODO: store comments separately to prevent all this cloning?
			let mut new_comments = record.comments.clone();
			new_comments.push((comment, _sender.clone()));
			<ProposalOf<T>>::insert(proposal_hash, ProposalRecord {
				comments: new_comments,
				..record
			});
			Self::deposit_event(RawEvent::NewComment(_sender, proposal_hash));
			Ok(())
		}

		/// Advance a governance proposal into the "voting" stage. Can only be
		/// performed by the original author of the proposal.
		pub fn advance_proposal(origin, proposal_hash: T::Hash) -> Result {
			let _sender = ensure_signed(origin)?;
			let record = <ProposalOf<T>>::get(&proposal_hash).ok_or("Proposal does not exist")?;

			// only permit original author to advance
			ensure!(record.author == _sender, "Proposal must be advanced by author");
			ensure!(record.stage == ProposalStage::PreVoting, "Proposal not in pre-voting stage");
			
			let transition_block = <system::Module<T>>::block_number() + Self::voting_time();
			<ProposalOf<T>>::insert(proposal_hash, ProposalRecord {
				stage: ProposalStage::Voting,
				transition_block: Some(transition_block),
				..record
			});
			<ActiveProposals<T>>::mutate(|proposals| proposals.push((proposal_hash, transition_block)));
			Self::deposit_event(RawEvent::VotingStarted(proposal_hash, transition_block));
			Ok(())
		}

		/// Submit or update a vote on a proposal. The proposal must be in the
		/// "voting" stage.
		pub fn submit_vote(origin, proposal_hash: T::Hash, vote: bool) -> Result {
			let _sender = ensure_signed(origin)?;
			let record = <ProposalOf<T>>::get(&proposal_hash).ok_or("Proposal does not exist")?;
			ensure!(record.stage == ProposalStage::Voting, "Proposal not in voting stage");

			if Self::vote_of((proposal_hash, _sender.clone())).is_none() {
				<ProposalVoters<T>>::mutate(proposal_hash, |voters| voters.push(_sender.clone()));
			}
			<VoteOf<T>>::insert((proposal_hash.clone(), _sender.clone()), vote);
			Self::deposit_event(RawEvent::VoteSubmitted(proposal_hash, _sender, vote));
			Ok(())
		}

		/// Check all active proposals to see if they're completed. If so, update
		/// them in storage and emit an event.
		fn on_finalise(n: T::BlockNumber) {
			let (finished, active): (Vec<_>, _) = <ActiveProposals<T>>::get()
				.into_iter()
				.partition(|(_, completion)| n >= *completion);
			
			<ActiveProposals<T>>::put(active);
			finished.into_iter().for_each(move |(completed_hash, _)| {
				// tally votes and update each finished proposal to "completed"
				if let Some(proposal) = <ProposalOf<T>>::get(completed_hash) {
					let (yes, no) = Self::tally_proposal(completed_hash);
					<ProposalOf<T>>::insert(completed_hash, ProposalRecord {
						stage: ProposalStage::Completed,
						transition_block: None,
						..proposal
					});
					Self::deposit_event(RawEvent::VotingCompleted(completed_hash, yes, no));
				}
			});
		}
	}
}

impl<T: Trait> Module<T> {
	/// Gets the first voter in the delegation chain of given account
	fn get_first_delegator(voters: &Vec<T::AccountId>, who: T::AccountId) -> T::AccountId {
		match <delegation::Module<T>>::delegate_of(&who) {
			Some(delegate) => match voters.iter().find(|&v| v == &delegate) {
				Some(voter) => voter.clone(),
				None => Self::get_first_delegator(voters, who),
			}
			None => who,
		}
	}

	/// Collect all pairs of (delegator, voter) for a given vote
	fn tally_delegation(voters: Vec<T::AccountId>) -> Vec<(T::AccountId, T::AccountId)> {
		// first, get a set of all "implicated" voters
		let mut all_children: Vec<T::AccountId> = voters
			.iter()
			.cloned()
			.flat_map(<delegation::Module<T>>::get_delegates_to)
			.collect();
		all_children.sort_unstable();
		all_children.dedup();

		// then, trace each "back up the tree" until we find an explicit voter
		return all_children
			.into_iter()
			.map(|c| (c.clone(), Self::get_first_delegator(&voters, c)))
			.collect();
	}

	/// Tally results given a proposal including delegations
	fn tally_proposal(proposal_hash: T::Hash) -> (usize, usize) {
		let mut yes = 0;
		let mut no = 0;
		let voters = <ProposalVoters<T>>::get(proposal_hash);
		let delegations = Self::tally_delegation(voters);
		delegations.iter().for_each(|(_, voter)| {
			if let Some(vote) = Self::vote_of((proposal_hash, voter.clone())) {
				if vote == true {
					yes += 1;
				} else {
					no += 1;
				}
			}
		});
		return (yes, no);
	}
}

decl_event!(
	pub enum Event<T> where <T as system::Trait>::Hash,
							<T as system::Trait>::AccountId,
							<T as system::Trait>::BlockNumber {
		/// Emitted at proposal creation: (Creator, ProposalHash)
		NewProposal(AccountId, Hash),
		/// Emitted at comment creation: (Commentor, ProposalHash)
		NewComment(AccountId, Hash),
		/// Emitted when voting begins: (ProposalHash, VotingEndTime)
		VotingStarted(Hash, BlockNumber),
		/// Emitted when a vote is submitted: (ProposalHash, Voter, Vote)
		VoteSubmitted(Hash, AccountId, bool),
		/// Emitted when voting is completed: (ProposalHash, YesVotes, NoVotes)
		VotingCompleted(Hash, usize, usize),
	}
);

decl_storage! {
	trait Store for Module<T: Trait> as Governance {
		/// The total number of proposals created thus far.
		pub ProposalCount get(proposal_count) : u32;
		/// A list of all extant proposals.
		pub Proposals get(proposals): Vec<T::Hash>;
		/// A list of active proposals along with the block at which they complete.
		pub ActiveProposals get(active_proposals): Vec<(T::Hash, T::BlockNumber)>;
		/// Amount of time a proposal remains in "Voting" stage, in blocks.
		pub VotingTime get(voting_time) config(): T::BlockNumber;
		/// Map for retrieving the information about any proposal from its hash.
		pub ProposalOf get(proposal_of): map T::Hash => Option<ProposalRecord<T::AccountId, T::BlockNumber>>;
		/// Map for retrieving the users who've voted on any particular proposal.
		pub ProposalVoters get(proposal_voters): map T::Hash => Vec<T::AccountId>;
		/// Map for retrieving the actual votes of users on any particular proposal.
		pub VoteOf get(vote_of): map (T::Hash, T::AccountId) => Option<bool>;
	}
}
