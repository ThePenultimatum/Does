
sealed trait Trie[+A] extends Product with Serializable {
  
  private val rootNodeInd: Long = -1L

  def isEmpty(t: Trie[_]): Boolean = {
    t match {
      case Empty => true
      case _ => false

    }
  }

  def isRoot[A](t: Trie[A]): Boolean = {
    t match {
      case Empty => false
      case TrieWithSubTries(_,i,_) => i == rootNodeInd
    }
  }

  def getNodeVal[A](t: TrieWithSubTries[A]): A = {
    t match {
      case TrieWithSubTries(n,_,_) => n
    }
  }

  def getNodeInd[A](t: TrieWithSubTries[A]): Long = {
    t match {
      case TrieWithSubTries(_,i,_) => i
    }
  }

  def getSubTries[A](t: TrieWithSubTries[A]): List[TrieWithSubTries[A]] = {
    t match {
      case TrieWithSubTries(_,_,r) => r
    }
  }
  
  def nodeValsEqual[A](t0: TrieWithSubTries[A],t1: TrieWithSubTries[A]): Boolean = {
    (t0,t1) match {
      case (TrieWithSubTries(n0,_,r),TrieWithSubTries(n1,_,_)) => n0 == n1
    }
  }
}

case object Empty extends Trie[Nothing]

case class TrieWithSubTries[A](nodeVal: A, nodeInd: Long, SubTries: List[TrieWithSubTries[A]]) extends Trie[A]
