import akka.actor._
import java.security.MessageDigest
object project1 extends App {
	//println("Enter number of zeroes")
	Global.start_num_zeroes = "0"*(args(0).toInt)
	Global.current_suffix_length=args(0).toInt -1
	val system=ActorSystem("PROJECT1")
	var workerList:List[ActorRef] = List()
	for(i<-1 to 22){
		var worker = system.actorOf(Props[Worker])
		workerList = workerList ::: List(worker)
	}
	val boss=system.actorOf(Props(new Boss(workerList)))
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

class Boss(workerList:List[ActorRef]) extends Actor{  
  var num_workers=1
  //var current_suffix_length:Int=1
  
  def receive={
    case "START"=>      
      for(i<-workerList){
      	i ! Global.current_suffix_length
      	Global.current_suffix_length+=1
      }
      
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
