package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Cons(_,t)=>t
    }
    
  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Cons(_,t)=> Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n<=0) l
    else l match{
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropWhile0[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) => if (f(h)) dropWhile(t, f) else l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def init2Help(cur: List[A]): List[A] = 
      cur match {
        case Cons(_,Nil) => List(buf.toList: buf.toList: _*)
        case Cons(h,t) => buf += h ; go(t)
      }
    init2Help()
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0) ( (_,accu)=>accu+1 ) 
  /* a function that consumes HEAD, returns accu+1 
   * where accu is 0 upon nil list */
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match{
      case Nil => z // zero val
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
      // l-> (cdr l), z->f(z,h)
      // apply f on zero-val and h first (1 stack deep)
      // a function that 

      /* [1 2 3 4]
       * tmp_1 = f(z, 1)
       * tmp_2 = f(tmp_1, 2)
       * tmp_3 = f(tmp_2, 3)
       * tmp_4 = f(tmp_3, 4)
       * tmp_5 = f(tmp_4, Nil) = tmp_4
       * res = tmp5 = tmp4
       * */
    }
  def sum3(l:List[Int]) = foldLeft(l,0)(_ + _) 
  // first add head with 0, then add next with head,
  // then add ... onto the accumulator
  //
  def product3(l:List[Int]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l:List[A]): Int=foldLeft(l,0)((soFar,_)=>soFar+1)
  /*length2([1 2 3 4])
   *
   * FL([1 2 3 4], 0)  
   * FL([2 3 4], 1)    
   * FL([3 4], 2)     
   * FL([4], 3)        
   * FL(Nil, 4)
   * 4   *
   * */
  def reverse[A](l:List[A]): List[A] =
    foldLeft(l, List[A]()) ((intermediate, h)=>Cons(h,intermediate))

  /*
   * foldRight on [1 2 3 4] would be do (after linearize the code)
   * -> f(1,f(2,f(3,f(4,z))))  | -> on foldLeft of the same 
   * which is,                 | [1 2 3 4] same list will be
   * 1. tmp_1 = f(4,z)         | 1. tmp_1 = f(z, 1)
   * 2. tmp_2 = f(3, tmp_1)    | 2. tmp_2 = f(tmp_1, 2)
   * 3. tmp_3 = f(2, tmp_2)    | 3. tmp_3 = f(tmp_2, 3)
   * 4. tmp_4 = f(1, tmp_3)    | 4. tmp_4 = f(tmp_3, 4)
   * 5. result=tmp_4           | 5. result= tmp_4 = f(tmp_4, Nil)
   *
   * observe with above, it is just work on fL on reverse order.
   * Also, 2 args of f would switch order
   * */
  def foldRightByFoldLeft[A,B](l:List[A], z:B)(f:(A,B)=>B): B=
    foldLeft(reverse(l), z)((arg1, arg2)=>f(arg2, arg1))


  def foldRightByFoldLeft_1[A,B](l: List[A], z:B)(f:(A,B)=>B): B=
    foldLeft(l, (b:B)=>b ) /* zero val is identity func.
     now fL still waiting for `f: BxA => B` 
       (f curr (car L)) -> newCurr
    */ ((g, a) => /* a func takes 2 args (g, a) and ret another func that
      takes 1 args, return g(f(a, b)) */ b => g(f(a,b))) 
      (z)
    
  def appendByFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((curr, h)=>Cons(h, curr))

  def appendNByFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((h, curr)=>Cons(h, curr))
  /* using foldright is actually faster in this case, 
     As the function itself is Cons(_,_)  
     
     let say, L has len(n), R has len(m), then at n-th call:
     we had L_1::L_2::...::L_n::R
     which, is exactly what we want. 

     So no need to do another n calls, sense it is already there.
     */

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append) // L1::L2::L3..::Ln::Nil
  
    foldLeft(l, Nil:List[A])((curr, h)=> match curr {
      case Nil => Cons(h, curr)
      case _ => append(curr, Cons(h, Nil))
    } /* Since Nil can't be at beginning at a list, 
        need to deal with 1 special case  */
  )

  def add1(l: List[Int]): List[Int] = 
    foldLeft(l, Nil:List[Int])((currL, h)=>Cons(h+1, currL))
  
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldLeft(l, Nil:List[B])( (currL, h)=>Cons(f(h), currL) )

  def flatMap[A,B](l: List[A])(f: A=>List[B]): List[B] = 
    concat(map(l)(f))
      

}