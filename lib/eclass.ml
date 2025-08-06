(*
use std::fmt::Debug;
use std::iter::ExactSizeIterator;

use crate::*;

/// An equivalence class of enodes.
#[non_exhaustive]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
pub struct EClass<L, D> {
    /// This eclass's id.
    pub id: Id,
    /// The equivalent enodes in this equivalence class.
    pub nodes: Vec<L>,
    /// The analysis data associated with this eclass.
    ///
    /// Modifying this field will _not_ cause changes to propagate through the e-graph.
    /// Prefer [`EGraph::set_analysis_data`] instead.
    pub data: D,
    /// The original Ids of parent enodes.
    pub(crate) parents: Vec<Id>,
}
*)
open! Core
open Id

module EClass : sig
  type ('l, 'd) t

  val get_id : ('l, 'd) t -> Id.t
  val get_data : ('l, 'd) t -> 'd
  val get_nodes : ('l, 'd) t -> 'l array
  val get_node : ('l, 'd) t -> int -> 'l
  val get_parents : ('l, 'd) t -> Id.t array
  val mapi_nodes : ('l, 'd) t -> (int -> 'l -> 'l) -> unit
  val map_node : ('l, 'd) t -> int -> ('l -> 'l) -> unit
  val find_nodes : ('l, 'd) t -> ('l -> bool) -> 'l option
  val filter_nodes : ('l, 'd) t -> ('l -> bool) -> 'l array
  val set_parents : ('l, 'd) t -> Id.t array -> unit
  val is_empty : ('l, 'd) t -> bool
end = struct
  type ('l, 'd) t = {
    id : Id.t;
    mutable nodes : 'l array;
    data : 'd;
    mutable parents : Id.t array;
  }

  let get_id (eclass : ('l, 'd) t) = eclass.id
  let get_data (eclass : ('l, 'd) t) = eclass.data
  let get_nodes (eclass : ('l, 'd) t) = eclass.nodes
  let get_parents (eclass : ('l, 'd) t) = eclass.parents
  let get_node (eclass : ('l, 'd) t) (pos : int) = eclass.nodes.(pos)

  let map_node (eclass : ('l, 'd) t) pos f =
    let node = eclass.nodes.(pos) in
    Array.set eclass.nodes pos (f node)

  let mapi_nodes (eclass : ('l, 'd) t) (f : int -> 'l -> 'l) =
    let new_nodes = Array.mapi ~f eclass.nodes in
    eclass.nodes <- new_nodes

  let find_nodes (eclass : ('l, 'd) t) (f : 'l -> bool) =
    Array.find eclass.nodes ~f

  let filter_nodes (eclass : ('l, 'd) t) (f : 'l -> bool) =
    Array.filter eclass.nodes ~f

  let set_parents (eclass : ('l, 'd) t) (new_parents : Id.t array) =
    eclass.parents <- new_parents

  let is_empty (eclass : ('l, 'd) t) = Array.is_empty eclass.nodes
end

(*


impl<L: Language, D> EClass<L, D> {
    /// Iterates over the childless enodes in this eclass.
    pub fn leaves(&self) -> impl Iterator<Item = &L> {
        self.nodes.iter().filter(|&n| n.is_leaf())
    }

    /// Asserts that the childless enodes in this eclass are unique.
    pub fn assert_unique_leaves(&self)
    where
        L: Language,
    {
        let mut leaves = self.leaves();
        if let Some(first) = leaves.next() {
            assert!(
                leaves.all(|l| l == first),
                "Different leaves in eclass {}: {:?}",
                self.id,
                self.leaves().collect::<crate::util::HashSet<_>>()
            );
        }
    }

    /// Run some function on each matching e-node in this class.
    pub fn for_each_matching_node<Err>(
        &self,
        node: &L,
        mut f: impl FnMut(&L) -> Result<(), Err>,
    ) -> Result<(), Err>
    where
        L: Language,
    {
        if self.nodes.len() < 50 {
            self.nodes
                .iter()
                .filter(|n| node.matches(n))
                .try_for_each(f)
        } else {
            debug_assert!(node.all(|id| id == Id::from(0)));
            debug_assert!(self.nodes.windows(2).all(|w| w[0] < w[1]));
            let mut start = self.nodes.binary_search(node).unwrap_or_else(|i| i);
            let discrim = node.discriminant();
            while start > 0 {
                if self.nodes[start - 1].discriminant() == discrim {
                    start -= 1;
                } else {
                    break;
                }
            }
            let mut matching = self.nodes[start..]
                .iter()
                .take_while(|&n| n.discriminant() == discrim)
                .filter(|n| node.matches(n));
            debug_assert_eq!(
                matching.clone().count(),
                self.nodes.iter().filter(|n| node.matches(n)).count(),
                "matching node {:?}\nstart={}\n{:?} != {:?}\nnodes: {:?}",
                node,
                start,
                matching.clone().collect::<HashSet<_>>(),
                self.nodes
                    .iter()
                    .filter(|n| node.matches(n))
                    .collect::<HashSet<_>>(),
                self.nodes
            );
            matching.try_for_each(&mut f)
        }
    }
}

*)
