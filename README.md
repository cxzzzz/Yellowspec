Yellowspec
=======================

#Yellowspec is a library which provide Bluespec-like expression for hardward based on chisel3. Just as Bluespec (a High-level Hardware Design Language), 
#Yellowspec using **Rules** and **Methods** to express synthesizable behavior, which are more powerful and abstract ways for data-driven circuit than always blocks and ports.

Yellowspec is a library providing software-like interface definitions and bindings for Module in chisel3.

Yellowspec using **Rules** and **Methods** to express synthesizable behavior, which are more powerful and abstract ways for data-driven circuit than always blocks and ports in chisel.
**Method** is the abstraction of the Module's interface and combinational logic behavior,simular to function definition in software. **Rule** is used to express sequencial logic , and can call Method in a way simular to function call.


## What does Yellowspec code look like?

Consider a module : read numbers from outside the module, multiply it by 2 and put the result to the queue.When needed,write out the results.

The code using Yellowspec is like this:
```scala

class Mult2(depth: Int) extends Module with Yellowspec {
	val io = IO(new Bundle {
		// declare a method Mult2 own
		val deq = ActionMethodIO(Void, UInt(8.W))
		// declare a method Mult2 use
		val enq = Flipped(ActionMethodIO(Void, UInt(8.W)))
	})

	val queue = Reg(Vec(depth, UInt(8.W)))
	val headPointer = Reg(UInt(3.W))
	val tailPointer = Reg(UInt(3.W))

	// bind io.deq to a method , which take a number from queue and return if queue is not empty
	io.deq := ActionMethod(headPointer =/= tailPointer) {
		(params:Void) => {
			tailPointer := tailPointer + 1.U
			queue(tailPointer)
		}
	}

	// when the rule queue is not full , read a number , multiply it by 2 and put it in to the queue
	rule(headPointer =/= tailPointer - 1.U) {
		headPointer := headPointer + 1.U
		queue(headPointer) := io.enq(Void) * 2.U
	}

}

```
#Module Mult2 has 2 methods : Enq ,deq. Method enq is called by Mult2 , providng data that Mult2 needs.

As you can see, instead of ports and when , this Module using method and rule.


## Getting Started
### Add Yellowspec To Dependencies

Clone this repo into a directory , then build and publish to your cache
```sh
git clone https://github.com/cxzzzz/Yellowspec.git
cd Yellowspec
sbt "publishLocal"
```   

Add dependencies into your project's build.sbt:
```scala
libraryDependencies += "org.cxzzzz" %% "yellowspec" % "0.+"
```

##  
