// NFAs in Scala using partial functions (returning
// sets of states)
//
// needs :load dfa.scala   for states


// type abbreviation for partial functions
type :=>[A, B] = PartialFunction[A, B]

// return an empty set when not defined
def applyOrElse[A, B](f: A :=> Set[B], x: A) : Set[B] =
  Try(f(x)) getOrElse Set[B]()


// NFAs
case class NFA[A, C](starts: Set[A],           // starting states
                     delta: (A, C) :=> Set[A], // transition function
                     fins:  A => Boolean) {    // final states 

  // given a state and a character, what is the set of 
  // next states? if there is none => empty set
  def next(q: A, c: C) : Set[A] = 
    applyOrElse(delta, (q, c))

  def nexts(qs: Set[A], c: C) : Set[A] =
    qs.flatMap(next(_, c))

  // given some states and a string, what is the set 
  // of next states?
  def deltas(qs: Set[A], s: List[C]) : Set[A] = s match {
    case Nil => qs
    case c::cs => deltas(nexts(qs, c), cs)
  }

  // is a string accepted by an NFA?
  def accepts(s: List[C]) : Boolean = 
    deltas(starts, s).exists(fins)

  // depth-first version of accepts
  def search(q: A, s: List[C]) : Boolean = s match {
    case Nil => fins(q)
    case c::cs => next(q, c).exists(search(_, cs))
  }

  def accepts2(s: List[C]) : Boolean =
    starts.exists(search(_, s))
}



// NFA examples

val nfa_trans1 : (State, Char) :=> Set[State] = 
  { case (Q0, 'a') => Set(Q0, Q1) 
    case (Q0, 'b') => Set(Q2) 
    case (Q1, 'a') => Set(Q1) 
    case (Q2, 'b') => Set(Q2) }

val nfa1 = NFA(Set[State](Q0), nfa_trans1, Set[State](Q2))

nfa1.accepts("aa".toList)             // false
nfa1.accepts("aaaaa".toList)          // false
nfa1.accepts("aaaaab".toList)         // true
nfa1.accepts("aaaaabbb".toList)       // true
nfa1.accepts("aaaaabbbaaa".toList)    // false
nfa1.accepts("ac".toList)             // false

nfa1.accepts2("aa".toList)             // false
nfa1.accepts2("aaaaa".toList)          // false
nfa1.accepts2("aaaaab".toList)         // true
nfa1.accepts2("aaaaabbb".toList)       // true
nfa1.accepts2("aaaaabbbaaa".toList)    // false
nfa1.accepts2("ac".toList)             // false




// subset constructions

def subset[A, C](nfa: NFA[A, C]) : DFA[Set[A], C] = {
  DFA(nfa.starts, 
      { case (qs, c) => nfa.nexts(qs, c) }, 
      _.exists(nfa.fins))
}

subset(nfa1).accepts("aa".toList)             // false
subset(nfa1).accepts("aaaaa".toList)          // false
subset(nfa1).accepts("aaaaab".toList)         // true
subset(nfa1).accepts("aaaaabbb".toList)       // true
subset(nfa1).accepts("aaaaabbbaaa".toList)    // false
subset(nfa1).accepts("ac".toList)             // false
