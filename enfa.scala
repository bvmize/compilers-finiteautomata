// epsilon NFAs...immediately translated into NFAs
// (needs :load dfa.scala 
//        :load nfa.scala in REPL)

// fixpoint construction
import scala.annotation.tailrec
@tailrec
def fixpT[A](f: A => A, x: A): A = {
  val fx = f(x)
  if (fx == x) x else fixpT(f, fx) 
}

// translates eNFAs directly into NFAs 
def eNFA[A, C](starts: Set[A],                     // starting states
               delta: (A, Option[C]) :=> Set[A],   // epsilon-transitions
               fins: A => Boolean) : NFA[A, C] = { // final states 

  // epsilon transitions
  def enext(q: A) : Set[A] = 
    applyOrElse(delta, (q, None))

  def enexts(qs: Set[A]) : Set[A] = 
    qs | qs.flatMap(enext(_))     // | is the set-union in Scala

  // epsilon closure
  def ecl(qs: Set[A]) : Set[A] = 
    fixpT(enexts, qs)

  // "normal" transitions
  def next(q: A, c: C) : Set[A] = 
    applyOrElse(delta, (q, Some(c)))

  def nexts(qs: Set[A], c: C) : Set[A] = 
    ecl(ecl(qs).flatMap(next(_, c)))

  // result NFA
  NFA(ecl(starts), 
      { case (q, c) => nexts(Set(q), c) }, 
      q => ecl(Set(q)) exists fins)
}


// eNFA examples
val enfa_trans1 : (State, Option[Char]) :=> Set[State] =
  { case (Q0, Some('a')) => Set(Q0)
    case (Q0, None) => Set(Q1, Q2)
    case (Q1, Some('a')) => Set(Q1)
    case (Q2, Some('b')) => Set(Q2) 
  }

val enfa1 = eNFA(Set[State](Q0), enfa_trans1, Set[State](Q2))


//
case object R1 extends State
case object R2 extends State
case object R3 extends State

val enfa_trans2 : (State, Option[Char]) :=> Set[State] =
  { case (R1, Some('b')) => Set(R3)
    case (R1, None) => Set(R2)
    case (R2, Some('a')) => Set(R1, R3) 
  }


val enfa2 = eNFA(Set[State](R1), enfa_trans1, Set[State](R3))
