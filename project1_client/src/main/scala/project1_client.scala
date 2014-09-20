import akka.actor._
import java.security.MessageDigest
//import akka.actor.ActorDSL._
object project1_client extends App {  
  implicit val system = ActorSystem("Client")
  
  val server = system.actorSelection("akka.tcp://PROJECT1@"+args(0)+":2552/user/Server") 
  val worker1 = system.actorOf(Props(new Worker1(server)))
  val worker2 = system.actorOf(Props(new Worker2(server)))
  val worker3 = system.actorOf(Props(new Worker3(server)))
  val worker4 = system.actorOf(Props(new Worker4(server)))
  val worker5 = system.actorOf(Props(new Worker5(server)))
  val worker6 = system.actorOf(Props(new Worker6(server)))
  val worker7 = system.actorOf(Props(new Worker7(server)))
  val worker8 = system.actorOf(Props(new Worker8(server)))
  val worker9 = system.actorOf(Props(new Worker9(server)))
  val worker10 = system.actorOf(Props(new Worker10(server)))
  val worker11 = system.actorOf(Props(new Worker11(server)))  
  val client = system.actorOf(Props(new Client(server,worker1,worker2,worker3,worker4,worker5,worker6,worker7,worker8,worker9,worker10,worker11)))  
  
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


class Client(server:ActorSelection,worker1:ActorRef,worker2:ActorRef,worker3:ActorRef,worker4:ActorRef,worker5:ActorRef,worker6:ActorRef,worker7:ActorRef,worker8:ActorRef,worker9:ActorRef,worker10:ActorRef,worker11:ActorRef) extends Actor{
  
  def receive={
    case "START"=>
      server ! "GIVE WORK"
    case Input(current_suffix_length,num_zeroes)=>
      var csl = current_suffix_length
      Global.start_num_zeroes = "0"*num_zeroes
      worker1 ! csl
      csl+=1     
      worker2 ! csl
      csl+=1
      worker3 ! csl
      csl+=1
      worker4 ! csl
      csl+=1
	  worker5 ! csl
      csl+=1
	  worker6 ! csl
      csl+=1     
	  worker7 ! csl
      csl+=1     
	  worker8 ! csl
      csl+=1     
	  worker9 ! csl
      csl+=1     
	  worker10 ! csl
      csl+=1 
	  worker11 ! csl
      csl+=1     	  
    context.stop(self)
      
  }
}

class Worker1(server:ActorSelection) extends Actor{
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

class Worker2(server:ActorSelection) extends Actor{
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
class Worker3(server:ActorSelection) extends Actor{
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
class Worker4(server:ActorSelection) extends Actor{
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

class Worker5(server:ActorSelection) extends Actor{
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

class Worker6(server:ActorSelection) extends Actor{
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
class Worker7(server:ActorSelection) extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Client Worker1 received "+csl)
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

class Worker8(server:ActorSelection) extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Client Worker1 received "+csl)
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

class Worker9(server:ActorSelection) extends Actor{
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

class Worker10(server:ActorSelection) extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Client Worker1 received "+csl)
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

class Worker11(server:ActorSelection) extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Client Worker1 received "+csl)
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

