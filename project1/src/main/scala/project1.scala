import akka.actor._
import java.security.MessageDigest
object project1 extends App {
	//println("Enter number of zeroes")
	Global.start_num_zeroes = "0"*(args(0).toInt)
	Global.current_suffix_length=args(0).toInt -1
	val system=ActorSystem("PROJECT1")
	val worker1=system.actorOf(Props[Worker])
	val worker2=system.actorOf(Props[Worker])
	val worker3=system.actorOf(Props[Worker])
	val worker4=system.actorOf(Props[Worker])
	val worker5=system.actorOf(Props[Worker])
	val worker6=system.actorOf(Props[Worker])
	val worker7=system.actorOf(Props[Worker])
	val worker8=system.actorOf(Props[Worker])
	val worker9=system.actorOf(Props[Worker])
	val worker10=system.actorOf(Props[Worker])
	val worker11=system.actorOf(Props[Worker])
        val worker12=system.actorOf(Props[Worker])
	val worker13=system.actorOf(Props[Worker])
	val worker14=system.actorOf(Props[Worker])
	val worker15=system.actorOf(Props[Worker])
	val worker16=system.actorOf(Props[Worker])
        val worker17=system.actorOf(Props[Worker])
	val worker18=system.actorOf(Props[Worker])
	val worker19=system.actorOf(Props[Worker])
	val worker20=system.actorOf(Props[Worker])
	val worker21=system.actorOf(Props[Worker])
	val worker22=system.actorOf(Props[Worke])
	val boss=system.actorOf(Props(new Boss(worker1,worker2,worker3,worker4,worker5,worker6,worker7,worker8,worker9,worker10,worker11,worker12,worker13,worker14,worker15,worker16,worker17,worker18,worker19,worker20,worker21,worker22)))
	var server=system.actorOf(Props[Server],name="Server")
	boss ! "START"
	server ! "Server Ready"
}

object Global {  
  var start_num_zeroes = ""
  var current_suffix_length:Int=1
  var char_set = (33 to 126).map(_.toChar).toList
  def getHash(text:String):String = {
		var hash:Array[Byte]=MessageDigest.getInstance("SHA-256").digest(text.getBytes)
		var ans:StringBuilder=new StringBuilder()  
		for(i<-hash)
		{
			ans.append(Integer.toString((i&0xff)+0x100,16).substring(1))
		}
		return ans.toString		
	}
}

class Boss(worker1:ActorRef,worker2:ActorRef,worker3:ActorRef,worker4:ActorRef,worker5:ActorRef,worker6:ActorRef,worker7:ActorRef,worker8:ActorRef,worker9:ActorRef,worker10:ActorRef,worker11:ActorRef,worker12:ActorRef,worker13:ActorRef,worker14:ActorRef,worker15:ActorRef,worker16:ActorRef,worker17:ActorRef,worker18:ActorRef,worker19:ActorRef,worker20:ActorRef,worker21:ActorRef,worker22:ActorRef) extends Actor{  
  var num_workers=1
  //var current_suffix_length:Int=1
  
  def receive={
    case "START"=>      
      worker1 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker2 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker3 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker4 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker5 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker6 ! Global.current_suffix_length
      Global.current_suffix_length+=1      
      worker7 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker8 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker9 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker10 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker11 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker12 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker13 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker14 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker15 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker16 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker17 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker18 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker19 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker20 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker21 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      worker22 ! Global.current_suffix_length
      Global.current_suffix_length+=1
      
    case "GIVE WORK"=>
      sender ! Global.current_suffix_length
      Global.current_suffix_length+=1
      
  }
}

case class Input(csl:Int,k:Int)

class Server extends Actor{
  def receive={
    case "GIVE WORK"=>
      sender ! Input(Global.current_suffix_length,Global.start_num_zeroes.length)
      Global.current_suffix_length+=11
    
    case "ClientWorkerDone"=>
      sender ! Global.current_suffix_length 
      Global.current_suffix_length +=1
    
    case "Server Ready"=>
    
    case ans=>
      println(ans)
    
    
    
  }
}

class Worker extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
      //println("Worker1 received "+csl)
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){        
        text = prefix + j.mkString("")        
        hash = Global.getHash(text)        
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
      sender ! "GIVE WORK"
  }
}
