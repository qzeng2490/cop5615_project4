//package example
import scala.actors.Actor
import scala.actors.Actor._
import java.io._

trait ActorLogging extends Actor {
    def logReact(handler: PartialFunction[Any, Unit]): Nothing = {
        val handler2: PartialFunction[Any, Unit] = {
            case Gossip(receive:String,source:String) =>
                if(Variable.flag == false){
                  printToFile("digraph g {")
                  Variable.flag = true
                }
                if(Network.result.length.toDouble/Network.allnodes.length.toDouble >= 0.9){     
                  printToFile("}")
                  System.exit(0)
                }
                var actorlog:String ='\t'+source+" -> "+receive+" [lable = "+Variable.lable+"] ;"
                Variable.lable= Variable.lable + 1 
               // println(actorlog)                
	        printToFile(actorlog)
                handler.apply(Gossip(receive:String,source:String))
            case a:Any  => handler.apply(a);
        }
        super.react(handler2)
    }

   
    def printToFile(str:String) {
        try { 
           var out:FileOutputStream  = new FileOutputStream("actorlog.txt",true);
           var osw:OutputStreamWriter = new OutputStreamWriter(out, "utf-8");
           var pw:PrintWriter  = new PrintWriter(osw);
           pw.println(str)
           pw.close()
           osw.close()
           out.close() 
        }finally {
          
        }
    }
}
object Variable {
  var lable:Int = 0
  var flag:Boolean = false
}

