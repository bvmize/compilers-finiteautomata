// Thompson Construction
// (needs  :load dfa.scala
//         :load nfa.scala
//         :load enfa.scala)


// states for Thompson construction
case class TState(i: Int) extends State

object TState {
  var counter = 0
  
  def apply() : TState = {
    counter += 1;
    new TState(counter - 1)
  }
}


// some types abbreviations
type NFAt = NFA[TState, Char]
type NFAtrans = (TState, Char) :=> Set[TState]
type eNFAtrans = (TState, Option[Char]) :=> Set[TState]


// for composing an eNFA transition with a NFA transition
implicit class RichPF(val f: eNFAtrans) extends AnyVal {
  def +++(g: NFAtrans) : eNFAtrans = 
  { case (q, None) =>  applyOrElse(f, (q, None)) 
    case (q, Some(c)) => applyOrElse(f, (q, Some(c))) | applyOrElse(g, (q, c))  }
}


// NFA that does not accept any string
def NFA_ZERO(): NFAt = {
  val Q = TState()
  NFA(Set(Q), { case _ => Set() }, Set())
}

// NFA that accepts the empty string
def NFA_ONE() : NFAt = {
  val Q = TState()
  NFA(Set(Q), { case _ => Set() }, Set(Q))
}

// NFA that accepts the string "c"
def NFA_CHAR(c: Char) : NFAt = {
  val Q1 = TState()
  val Q2 = TState()
  NFA(Set(Q1), { case (Q1, d) if (c == d) => Set(Q2) }, Set(Q2))
}

// sequence of two NFAs
def NFA_SEQ(enfa1: NFAt, enfa2: NFAt) : NFAt = {
  val new_delta : eNFAtrans = 
    { case (q, None) if enfa1.fins(q) => enfa2.starts }
  
  eNFA(enfa1.starts, new_delta +++ enfa1.delta +++ enfa2.delta, 
       enfa2.fins)
}

// alternative of two NFAs
def NFA_ALT(enfa1: NFAt, enfa2: NFAt) : NFAt = {
  val new_delta : NFAtrans = { 
    case (q, c) =>  applyOrElse(enfa1.delta, (q, c)) | 
                    applyOrElse(enfa2.delta, (q, c)) }
  val new_fins = (q: TState) => enfa1.fins(q) || enfa2.fins(q)

  NFA(enfa1.starts | enfa2.starts, new_delta, new_fins)
}

// star of a NFA
def NFA_STAR(enfa: NFAt) : NFAt = {
  val Q = TState()
  val new_delta : eNFAtrans = 
    { case (Q, None) => enfa.starts
      case (q, None) if enfa.fins(q) => Set(Q) }

  eNFA(Set(Q), new_delta +++ enfa.delta, Set(Q))
}



// regular expressions
abstract class Rexp
case object ZERO extends Rexp                    // matches nothing
case object ONE extends Rexp                     // matches the empty string
case class CHAR(c: Char) extends Rexp            // matches a character c
case class ALT(r1: Rexp, r2: Rexp) extends Rexp  // alternative
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp  // sequence
case class STAR(r: Rexp) extends Rexp            // star




// thompson construction 
def thompson (r: Rexp) : NFAt = r match {
  case ZERO => NFA_ZERO()
  case ONE => NFA_ONE()
  case CHAR(c) => NFA_CHAR(c)  
  case ALT(r1, r2) => NFA_ALT(thompson(r1), thompson(r2))
  case SEQ(r1, r2) => NFA_SEQ(thompson(r1), thompson(r2))
  case STAR(r1) => NFA_STAR(thompson(r1))
}

//optional regular expression (one or zero times)
def OPT(r: Rexp) = ALT(r, ONE)

//n-times regular expression (explicitly expanded)
def NTIMES(r: Rexp, n: Int) : Rexp = n match {
  case 0 => ONE
  case 1 => r
  case n => SEQ(r, NTIMES(r, n - 1))
}


def tmatches(r: Rexp, s: String) : Boolean =
  thompson(r).accepts(s.toList)

def tmatches2(r: Rexp, s: String) : Boolean =
  thompson(r).accepts2(s.toList)

// dfa via subset construction
def tmatches_dfa(r: Rexp, s: String) : Boolean =
  subset(thompson(r)).accepts(s.toList)

// Test Cases


// the evil regular expression  a?{n} a{n}
def EVIL1(n: Int) : Rexp = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))

// the evil regular expression (a*)*b
val EVIL2 : Rexp = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

//for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

// the size of the NFA can be large, 
// thus slowing down the breadth-first search

for (i <- 1 to 13) {
  println(i + ": " + "%.5f".format(time_needed(2, tmatches(EVIL1(i), "a" * i))))
}

for (i <- 1 to 100 by 5) {
  println(i + " " + "%.5f".format(time_needed(2, tmatches(EVIL2, "a" * i))))
}


// the backtracking needed in depth-first search 
// can be painfully slow

for (i <- 1 to 8) {
  println(i + " " + "%.5f".format(time_needed(2, tmatches2(EVIL2, "a" * i))))
}



// while my thompson-enfa-subset-partial-function-chain
// is probably not the most effcient way to obtain a fast DFA 
// (the test below should be much faster with a more direct 
// construction), in general the DFAs can be slow because of 
// the state explosion in the subset construction

for (i <- 1 to 13) {
  println(i + ": " + "%.5f".format(time_needed(2, tmatches_dfa(EVIL1(i), "a" * i))))
}

for (i <- 1 to 100 by 5) {
  println(i + " " + "%.5f".format(time_needed(2, tmatches_dfa(EVIL2, "a" * i))))
}
