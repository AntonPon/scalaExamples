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

  case class Sum(lOp: Expr, rOp: Expr) extends Expr

  case class Number(n: Int) extends Expr

  case class Booleans(b: Boolean) extends Expr

  case class Var(name: String) extends Expr

  case class Greater(lOp: Expr, rOp: Expr) extends Expr

  case class Prod(lOp: Expr, rOp: Expr) extends Expr

  case class IfElse(statement: Expr, trueCl: Expr, falseCl: Expr) extends Expr


   sealed trait Stat{

   }

   case class DoNothing() extends Stat
   case class Assign(name: String, expr: Expr) extends Stat
   case class IfElse2(expr: Expr, trueCl: Stat, falseCl: Stat) extends Stat
   case class Seq(stats: Stat*) extends Stat
   case class While(expr: Expr, seq: Seq) extends Stat

  final class Machine{
    def run(stat: Stat, env: Map[String, Any]):Map[String, Any] = {
      //println(expr)
      def runExpr(expr: Expr, env: Map[String, Any]):Expr = {
        println(expr)
        if (expr.isReduciable)
          runExpr(reduce(expr, env), env)
        else expr
      }


      stat match {
        case DoNothing() => env
        case Assign(name, expr) =>{
          val newVar = runExpr(expr, env)
          newVar match {
            case Number(n) => env ++ Map(name -> n)
            case Booleans(b) => env ++ Map(name -> b)

          }


        }
        case IfElse2(st, trueCl, falseCL) => {
          val stat:Expr = runExpr(st, env)
          require(!isNumber(stat), "the statement is not Booleans")
          if (getBooleans(stat)){
            run(trueCl, env)
          }else{
            run(falseCL, env)
          }
        }
        case Seq(rest@_*) =>{
          if(rest.length > 0 ){run(Seq(rest.tail : _*), run(rest.head, env))}
          else {env}
        }
        case While(expr, seq) => {
          println()

          val stat:Expr = runExpr(expr, env)
          require(!isNumber(stat), "the statement is not Booleans")
          if (getBooleans(stat)){
            run(While(expr, seq), run(seq, env))
          }
          else {env}
        }
      }
      }


    def reduce(expr: Expr, env: Map[String, Any]):Expr = {
     expr match {
       case Number(_) => expr
       case Var(name) => {
         env(name) match {
           case x:Int => Number(x)
           case x:Boolean => Booleans(x)
         }
       }
       case Sum(lOp, rOp) => {
         if (lOp.isReduciable) Sum(reduce(lOp, env), rOp)
         else if (rOp.isReduciable) Sum(lOp, reduce(rOp, env))
         else (expr.eval)
       }
       case Prod(lOp, rOp) => {
         if (lOp.isReduciable) Prod(reduce(lOp, env), rOp)
         else if (rOp.isReduciable) Prod(lOp, reduce(rOp, env))
         else (expr.eval)
       }
       case Greater(lOp, rOp) => {
         if (lOp.isReduciable) Greater(reduce(lOp, env), rOp)
         else if (rOp.isReduciable) Greater(lOp, reduce(rOp, env))
         else (expr.eval)
       }
       case IfElse(st, fsCl, scCl) => {
         if (st.isReduciable) IfElse(reduce(st, env), fsCl, scCl)
         else reduce(expr.eval, env)
       }
     }
     }

  }

  val a =   new Prod(new Sum(new Number(3), new Var("x")), new Number(10))
  val b = new  Prod(a, Sum(Number(25), Number(16)))
  val c = new Greater(new  Prod(a, Sum(Number(25), Number(16))), new Number(40))
  val d = new IfElse(c, a,b)
   val l = new While(new Greater(new Var("x"), new Number(34)), new Seq(Assign("x", Sum(new Var("x"), new Number(-1)))))


   //(new Machine).run(Assign("x1", d), Map("x" -> 15))
   (new Machine).run(l, Map("x"->40))//(Seq(Assign("x", Number(23)), Assign("z", Number(45))), Map("x" -> 15))
}


