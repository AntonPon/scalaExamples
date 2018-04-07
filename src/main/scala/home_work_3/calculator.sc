
{
  sealed trait Expr {

    def isReduciable: Boolean = this match {
      case Number(_) => false
      case Sum(_, _) => true
      case Prod(_, _) => true
      case Var(_) => true
    }

    def eval:Int = {
      this match{
        case Number(n) => n
        case Prod(lOp, rOp) => lOp.eval * rOp.eval
        case Sum(lOp, rOp) => lOp.eval + rOp.eval
      }
    }



    def show(e: Expr): String ={
      def sumMatcher(e: Expr) = {
        e match {
          case Number(_) => show(e)
          case Sum(_, _) => "(" + show(e) + ")"
          case Prod(_, _) => show(e)
          case Var(_) => show(e)
        }
      }
      e match {
        case Number(n) => n + ""
        case Var(name) => name
        case Sum(lOp, rOp) => show(lOp) + " + " + show(rOp)
        case Prod(lOp, rOp) => sumMatcher(lOp) + " * " +sumMatcher(rOp)
      }
    }

    override def toString = show(this)

  }

  case class Sum(lOp: Expr, rOp: Expr) extends Expr

  case class Number(n: Int) extends Expr

  case class Var(name: String) extends Expr

  case class Prod(lOp: Expr, rOp: Expr) extends Expr



  val a =   new Prod(new Sum(new Number(3), new Var("x")), new Number(10))
  val b = Prod(a, Sum(Number(25), Number(16)))


  final class Machine{
    def run(expr: Expr, env: Map[String, Int]):Expr = {
      println(expr)
      if (expr.isReduciable)
        run(reductionStep(expr, env), env)
      else expr
    }

    def reductionStep(expr: Expr, env: Map[String, Int]):Expr = {
     expr match {
       case Number(_) => expr
       case Var(name) => Number(env(name))
       case Sum(lOp, rOp) => {
         if (lOp.isReduciable) Sum(reductionStep(lOp, env), rOp)
         else if (rOp.isReduciable) Sum(lOp, reductionStep(rOp, env))
         else Number(expr.eval)
       }
       case Prod(lOp, rOp) =>{
         if (lOp.isReduciable) Prod(reductionStep(lOp, env), rOp)
         else if (rOp.isReduciable) Prod(lOp, reductionStep(rOp, env))
         else Number(expr.eval)
       }
     }
     }

  }

 println( (new Machine).run(b, Map("x"->15)))

}
