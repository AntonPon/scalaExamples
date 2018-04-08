
{
  sealed trait Expr {

    def isReduciable: Boolean = this match {
      case Number(_) => false
      case Booleans(_) => false
      case Var(_) => true
      case Sum(_, _) => true
      case Prod(_, _) => true
      case Greater(_, _) => true
      case IfElse(_, _, _) => true
    }



    def eval: Expr = {
      def getNumber(e: Expr): Int = {
        e match {
          case Number(n) => n
        }
      }

      def getBooleans(e: Expr): Boolean = {
        e match {
          case Booleans(b) => b
        }
      }
      def isNumber(e: Expr): Boolean = {
        e match {
        case Number(_) => true
        case Booleans(_) => false
        }
      }
      this match{
        case Number(_) => this
        case Booleans(_) => this
        case Prod(lOp, rOp) => {
          if (!lOp.isReduciable) require(isNumber(lOp), "left operand is not Number type")
          if (!rOp.isReduciable) require(isNumber(rOp), "right operand is not Number type")
          new Number(getNumber(lOp)*getNumber(rOp))
        }
        case Sum(lOp, rOp) => {
          if (!lOp.isReduciable) require(isNumber(lOp), "left operand is not Number type")
          if (!rOp.isReduciable) require(isNumber(rOp), "right operand is not Number type")
          new Number(getNumber(lOp) + getNumber(rOp))
        }
        case Greater(lOp, rOp) => {
          if (!lOp.isReduciable) {require(isNumber(lOp), "left operand is not Number type")}
          if (!rOp.isReduciable){ require(isNumber(rOp), "right operand is not Number ty}pe")}
          new Booleans(getNumber(lOp) > getNumber(rOp))
        }
        case IfElse(st, trCl, flCl) => {
          require(!isNumber(st), "the statement is not Booleans")
          if (getBooleans(st)) trCl
          else flCl
        }
      }
    }



    def show(e: Expr): String ={
      def matcher(e: Expr) = {
        e match {
          case Number(n) => n + ""
          case Booleans(b) => b + ""
          case Sum(_, _) => "(" + show(e) + ")"
          case Prod(_, _) => show(e)
          case Var(n) => n
          case Greater(_,_) => "("+ show(e) + ")"
        }
      }
      e match {
        case Number(_) => matcher(e)
        case Booleans(_) => matcher(e)
        case Var(_) => matcher(e)
        case Sum(lOp, rOp) => show(lOp) + " + " + show(rOp)
        case Greater(lOp, rOp) => show(lOp) + " > " + show(rOp)
        case Prod(lOp, rOp) => matcher(lOp) + " * " +matcher(rOp)
        case IfElse(st, trCl, flCl) => "if (" + show(st) + ") " + show(trCl) + "\n" + "else " + show(flCl)
      }
    }

    override def toString = show(this)

  }

  case class Sum(lOp: Expr, rOp: Expr) extends Expr

  case class Number(n: Int) extends Expr

  case class Booleans(b: Boolean) extends Expr

  case class Var(name: String) extends Expr

  case class Greater(lOp: Expr, rOp: Expr) extends Expr

  case class Prod(lOp: Expr, rOp: Expr) extends Expr

  case class IfElse(statement: Expr, trueCl: Expr, falseCl: Expr) extends Expr





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
         else (expr.eval)
       }
       case Prod(lOp, rOp) => {
         if (lOp.isReduciable) Prod(reductionStep(lOp, env), rOp)
         else if (rOp.isReduciable) Prod(lOp, reductionStep(rOp, env))
         else (expr.eval)
       }
       case Greater(lOp, rOp) => {
         if (lOp.isReduciable) Greater(reductionStep(lOp, env), rOp)
         else if (rOp.isReduciable) Greater(lOp, reductionStep(rOp, env))
         else (expr.eval)
       }
       case IfElse(st, fsCl, scCl) => {
         if (st.isReduciable) IfElse(reductionStep(st, env), fsCl, scCl)
         else reductionStep(expr.eval, env)
       }
     }
     }

  }

  val a =   new Prod(new Sum(new Number(3), new Var("x")), new Number(10))
  val b = new  Prod(a, Sum(Number(25), Number(16)))
  val c = new Greater(new  Prod(a, Sum(Number(25), Number(16))), new Number(40))
  val d = new IfElse(c, a,b)
 (new Machine).run(d, Map("x" -> 15))
}
