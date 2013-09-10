package math.lp

trait Domains {
  protected type Domain[A] = Set[A]

  protected def domain[A](as: Set[A]): Domain[A] = as
}

trait Vectors {
  self: Domains =>

  protected trait Vector[A, C] extends ((A) => C){
    def domain: Domain[A]

    def get: (A) => C

    def apply(a: A) = get(a)
  }

  protected def sparseVector[A, C: Numeric](d: Domain[A], m: Map[A, C]): Vector[A, C] = new Vector[A, C] {
    def domain: Domain[A] = d

    def get = m.getOrElse(_, implicitly[Numeric[C]].zero)
  }
}


trait Matrices {
  self: Domains =>

  protected type Data[A, B, C] = Map[(A,B), C]

  protected trait Matrix[A, B, C] extends (((A, B)) => C) {
    def domains: (Domain[A], Domain[B])

    def get: ((A, B)) => C

    def apply(k: (A, B)) = get(k)
  }


  protected def matrix[A, B, C : Numeric](ds: (Domain[A], Domain[B]), d: Data[A, B, C]): Matrix[A, B, C] = new Matrix[A, B, C] {
    def domains = ds

    def data = d

    def get = data.getOrElse(_, implicitly[Numeric[C]].zero)
  }
}

