//package example

import scala.actors.Actor
import scala.actors.Actor._

import scala.util.Random
import collection.immutable._

case class Gossip(receive:String,source:String)
case class Initnode(i:Int,neighbors0:List[Int],allnodes:List[GossipNode])
case class InitNet(allnodes:List[GossipNode])
case class id(i:Int);

object project4{
	def main(args : Array[String]){
		var numNodes = 300;
		
		var allnodes:List[GossipNode] = Nil;
		
		
		var i:Int =0;
		while(i<numNodes){
			var gossipnode = new GossipNode()
			allnodes ::= gossipnode;
			gossipnode.start();
			i = i+1;
		}
		
		var j:Int =0;
		while(j < allnodes.length){
			var neighbors:List[Int] = Nil;
			var k:Int = 0;
			
			while (k < allnodes.length){
				if(k != j){
					neighbors ::= k;
				}
				k = k+1;
			}
			
			allnodes(j) ! Initnode(j,neighbors,allnodes);
			j = j+1;
			
		}
		
		Network.start()
		
		Network ! InitNet(allnodes)
		
		allnodes(0) ! Gossip("0","start")
		
	}
}


object Network extends Actor{
	var allnodes:List[GossipNode] = Nil;
	var result:List[Int] = Nil;
	
	def act(){
		loop{
			react{
				case InitNet(allnodes0:List[GossipNode]) =>{
					allnodes = allnodes0;
	            		}
				case id(i) =>{
	               			var flag = false
	              		      //println("number :"+i)
	            			if(result.length == 0) result ::=i
	                		for(c<-0 until result.length){
	                   		   if( result(c) == i) flag = true 
	                		}
	                		if(!flag) result ::= i
	                		//println("result:   "+result+"\tlenghth:   "+result.length)
				}				
			}
				
		}
	}
}


class GossipNode extends Actor with ActorLogging{
	var gossipcount:Int =0;
	var rumorTermination:Int =10;
	var allnodes:List[GossipNode] = Nil;
	
	var neighbors:List[Int] = Nil
	
	var nodeid:Int =0;
	def act(){
		loop{
			logReact{
				case Initnode(i:Int,neighbors0:List[Int],allnodes0:List[GossipNode]) =>{
					nodeid = i;
					neighbors = neighbors0;
					allnodes = allnodes0;
					//println("nodeid: "+nodeid+"neighbors: "+neighbors)
				}
				case Gossip(receive:String,source:String) =>{
					var randomnode = 0 
					gossipcount = gossipcount + 1
					Network ! id(nodeid) 
					Thread.sleep(5)
					randomnode = Random.nextInt(neighbors.length)
					//println("nodeid: "+nodeid+"neighbors: "+neighbors+"rand: "+randomnode)
					allnodes(neighbors(randomnode)) ! Gossip(neighbors(randomnode).toString,this.nodeid.toString)
					
				}
			}
		}
	}
}
