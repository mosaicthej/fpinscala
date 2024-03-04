def map[B](f: A => B): Either[E, B] = 
  this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  
def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
  this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
  this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
  Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)

def map2_0[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
  Either[EE, C] = 
  this flatMap (aa => b map (bb => f(aa, bb)))

def map3[EE >: E, B, C, D](b: Either[EE, B], c: Either[EE, C])
  (f: (A, B, C) => D): Either[EE, D] = 
  for { a <- this; b1 <- b; c1 <- c } yield f(a,b1,c1)

def map3_0[EE >: E, B, C, D](b: Either[EE, B], c: Either[EE, C])
  (f: (A, B, C) => D): Either[EE, D] = 
  this flatMap (aa => b flatMap (bb => c map (cc => f(aa, bb, cc))))