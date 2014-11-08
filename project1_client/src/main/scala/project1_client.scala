import akka.actor._
import java.security.MessageDigest
//import akka.actor.ActorDSL._
object project1_client extends App {  
  implicit val system = ActorSystem("Client")
  
  val server = system.actorSelection("akka.tcp://PROJECT1@"+args(0)+":2552/user/Server") 
  var workerList:List[ActorRef] = List()
  for(i<-1 to 11){
	var worker = system.actorOf(Props(new Worker(server)))
	workerList = workerList ::: List(worker)
  }
  val client = system.actorOf(Props(new Client(server,workerList)))  
  
  client ! "START"  
}

object Global {  
  var start_num_zeroes=""
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

case class Input(csl:Int,k:Int)


class Client(server:ActorSelection,workerList:List[ActorRef]) extends Actor{
  
  def receive={
    case "START"=>
      server ! "GIVE WORK"
    case Input(current_suffix_length,num_zeroes)=>
      var csl = current_suffix_length
      Global.start_num_zeroes = "0"*num_zeroes
      for(worker<-workerList){
      	worker ! csl
      	csl += 1
      }
    context.stop(self)
      
  }
}

class Worker(server:ActorSelection) extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Client Worker1 received "+csl)
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){        
        text = prefix + j.mkString("")        
        hash = Global.getHash(text)        
        if(hash.startsWith(Global.start_num_zeroes)){
          var ans:String=text+"\t"+hash
          server ! ans
          //println(text + "\t"+ hash)
        }
      }
    }
      server ! "ClientWorkerDone"
  }
}
