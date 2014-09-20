import akka.actor._
import java.security.MessageDigest
object project1 extends App {
	//println("Enter number of zeroes")
	Global.start_num_zeroes = "0"*(args(0).toInt)
	Global.current_suffix_length=args(0).toInt -1
	val system=ActorSystem("PROJECT1")	
	val worker1=system.actorOf(Props[Worker1])
	val worker2=system.actorOf(Props[Worker2])
	val worker3=system.actorOf(Props[Worker3])
	val worker4=system.actorOf(Props[Worker4])
	val worker5=system.actorOf(Props[Worker5])
	val worker6=system.actorOf(Props[Worker6])
	val worker7=system.actorOf(Props[Worker7])
	val worker8=system.actorOf(Props[Worker8])
	val worker9=system.actorOf(Props[Worker9])
	val worker10=system.actorOf(Props[Worker10])
	val worker11=system.actorOf(Props[Worker11])
        val worker12=system.actorOf(Props[Worker12])
	val worker13=system.actorOf(Props[Worker13])
	val worker14=system.actorOf(Props[Worker14])
	val worker15=system.actorOf(Props[Worker15])
	val worker16=system.actorOf(Props[Worker16])
        val worker17=system.actorOf(Props[Worker17])
	val worker18=system.actorOf(Props[Worker18])
	val worker19=system.actorOf(Props[Worker19])
	val worker20=system.actorOf(Props[Worker20])
	val worker21=system.actorOf(Props[Worker21])
	val worker22=system.actorOf(Props[Worker22])
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

class Worker1 extends Actor{
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

class Worker2 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker2 received "+csl)
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

class Worker3 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker3 received "+csl)
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

class Worker4 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker4 received "+csl)
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

class Worker5 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker5 received "+csl)
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

class Worker6 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker6 received "+csl)
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


class Worker7 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker7 received "+csl)
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

class Worker8 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Worker8 received "+csl)
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

class Worker9 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
    //  println("Worker9 received "+csl)
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

class Worker10 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker10 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker10 has completed")
      sender ! "GIVE WORK"
  }
}

class Worker11 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker11 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker11 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker12 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker12 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker12 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker13 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker13 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker13 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker14 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker14 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker14 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker15 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker15 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker15 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker16 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker16 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker16 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker17 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker17 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker17 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker18 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker18 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker18 has completed")
      sender ! "GIVE WORK"
  }
}



class Worker19 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker19 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker19 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker20 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker20 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker20 has completed")
      sender ! "GIVE WORK"
  }
}


class Worker21 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker21 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker21 has completed")
      sender ! "GIVE WORK"
  }
}



class Worker22 extends Actor{
  var prefix="rkouj"
  var text = ""
  var hash:String = ""
  def receive={
    case csl:Int=>
     // println("Worker22 received "+csl)
      //var char_set:String=Global.char_set//*csl
    for(i<-Global.char_set.combinations(csl)){
      for(j<-i.permutations){
        text = prefix + j.mkString("")
        hash = Global.getHash(text)
        if(hash.startsWith(Global.start_num_zeroes)){
          println(text + "\t"+ hash)
        }
      }
    }
    println("Worker22 has completed")
      sender ! "GIVE WORK"
  }
}






