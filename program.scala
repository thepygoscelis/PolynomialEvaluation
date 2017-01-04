/*
 Author  : Charlie Hicks
 Purpose : Analyze the difference in four algorithms used to evaluate polynomials. 
 */

import math.{cos, sin, Pi,pow,round}
import scala.io.StdIn.{readLine, readInt}
import scala.util.Random
import scala.io.Source
import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.compat.Platform

/*
Represents Complex numbers with operatiosn for addition, subtraction, and multiplication 
 */
class Complex(var real:Double, var imaginary:Double){
  def this() = this(0,0);
  def +(that:Complex):Complex = {
    var a = new Complex()

    a.real = real + that.real
    a.imaginary = imaginary + that.imaginary
    a
  }
  def -(that:Complex):Complex = {
    new Complex(real-that.real,imaginary-that.imaginary);
  }
  def *(that:Complex):Complex = {
    new Complex(((real * that.real) - (imaginary * that.imaginary)), ((real*that.imaginary) + (imaginary * that.real)))
  } 
  override def toString():String = {
    "(" + real + "," + imaginary + ")"
  }
}

object i extends Complex{
  real = 0
  imaginary = 1
}


//below we represent your standard features like reading and writing a file. Also things like generating
//the roots of unity will be stored here. 
object Util {
  //This writes the complex numbers to a file for later retrival.
  def writeToFile(file:String,power:Int,poly:Array[Complex]) = {
    //opena writer
    val writer = new PrintWriter(new File(file))
    //First we want the number of complex numbers written
    writer.println(power.toString)
    //then we simply write the complex out in the form of a,b from the form a+bi
    for(i <- 0 until poly.length){
      writer.println(poly(i).real + "," + poly(i).imaginary)
    }
    //close our writer out
    writer.close
  }
  //This generates n complex numbers of the form (a,0) where a is 1-10
  def generateRandomPolynomial(n:Int) = {
    //Make the array
    var coefficients = new Array[Complex](n)
    //    var i = 0
    //Now genrate using the Random to generate a int then mod 10 + 1 for our range
    for(i <- 0 until n){
      coefficients(i) = new Complex((Random.nextInt()%10)+1,0)
    }
    //return our array
    coefficients
  }
  //This function with take a double and round it to p places after decimal.
  //pointsOfPercision(10.212,2) => 10.21
  def pointsOfPercision(x:Double,p:Int) = {
    round(x*pow(10,p))/(pow(10,p).toDouble)

  }
  //This function will generate the n roots of unity the first root is always 1
  def rootsOfUnity(n:Int) = {
    var roots = new Array[Complex](n)
    var a = 0
    //roots of unity calculated using Euler's identity that's why we use cos and sin
    //root(i) = cos((2*i*Pi)/n) + i * sin((2*i*Pi/n)) 
    for(a <- 0 until n){
      var T = (2*a*Pi)/n
      val c = new Complex(pointsOfPercision(cos(T),7),0)
      val s = new Complex(0,pointsOfPercision(sin(T),7))
      roots(a) = c + s
    }
    roots
  }
  //reads complex array from file of format number of complex then a,b for each complex
  //return Option[None] if invalid
  def readFromFile(filename:String):Option[Array[Complex]] = {

    try{
      //make sure that valid format exists and if fails return Option[None]
      val file = Source.fromFile(filename).getLines
      val total = file.next.toInt

      if(total < 0){
        return None
      }
      else{
        var returnValues = new Array[Complex](total)
        var a = 0

        for( a <- 0 until total){
          val values = file.next.split(",").map{ (x) => { x.toDouble}}
          returnValues(a) = new Complex(values(0),values(1))
        }
        return Some(returnValues)
      }

    } catch {
      case _:Throwable => return None
    }


  }


}


//Below is an object which will contain four methods representing
//the algorithms we choose to test.
object Algorithms{
  //This is a direct an easy to understand algorithm
  def runNaiveAlgorithm(degree:Int, polynomial:Array[Complex],operations:Array[Long]) = {
    //get out roots of unity
    val roots = Util.rootsOfUnity(degree)
    //There will be n answers for every root of unity
    val answers = Array.fill(degree)(new Complex(0,0))
    for(i <- roots.indices){
      //for each answer the first term of polynomial x^0 will just be the term
      var sum = polynomial(0)
      //take each term after the first
      for( j <- 1 until polynomial.length){
        //raise term to power it has to at least be 1
        var term = roots(i)
        //then raise it to the power that it should i.e. 3rd term is 2nd power so initial then 1 until 3 will run once
        for(k <- 1 until j){
          //up 1 operation for multiplication
          operations(0) = operations(0) +1
          term = term * roots(i)
        }
        //multiply the polynomial term by the root raised to the jth power then add into sum.
        operations(0) = operations(0) + 1
        sum += term * polynomial(j)

      }
      answers(i) = sum
    }
    //return answers array
    answers
  }
  //Using horners method evaluation a polynomial
  //has built in calculations for operations
  def hornersMethod(degree:Int, polynomial:Array[Complex],operations:Array[Long]) = {
    val roots = Util.rootsOfUnity(degree)
    val answers = new Array[Complex](degree)
    for(i <- roots.indices){
      var sum = new Complex(0,0)
      //we work backwards her from last towards beginning of list
      for(j <- (degree-1) to 1 by -1){
        //
        operations(1) = operations(1) + 1
        sum = sum + polynomial(j)
        sum = roots(i) * sum
      }
      sum += polynomial(0)
      answers(i) = sum
    }
    answers
  }

  /*
   recursive call of the recusion for decreasing by half.
   by squaring repeatedly 
   */
  def decreaseByHalfRecursion(power:Int,base:Complex,operations:Array[Long]) : Complex = {
    
    if(power == 1){
      base
    }
    else{
      //if the power is divisable by 2 just square ther result of the recusion
      if(power%2 == 0){
        val result = decreaseByHalfRecursion(power/2,base,operations)
        operations(2) = operations(2) + 1
        result * result
      }
      //if the power is not divisable by 2 then multiply the square of the result by the base 
      else{
        val result = decreaseByHalfRecursion((power-1)/2,base,operations)
        operations(2) = operations(2) + 2
        //account for the odd power
        result*result*base
      }
    }
  }

  /*
   decrease by half works by reduction: reducing the power by scaling down by 2 
   */
  def decreaseByHalf(degree:Int,polynomial:Array[Complex],operations:Array[Long]) = {
    val roots = Util.rootsOfUnity(degree)
    val answers = new Array[Complex](degree)
    for(i <- roots.indices){
      var sum = polynomial(0)
      for(j <- 1 until polynomial.length){
        operations(2) = operations(2) + 1
        sum += polynomial(j) * decreaseByHalfRecursion(j,roots(i),operations)
      }
      answers(i) = sum
    }
    answers
  }
  /*
   This is an implementation of FFT 
   The computation is a tree which first breaks down the list until of 1 
   Then recombines them using the FFT method
   */

  def fft(degree:Int,polynomial:Array[Complex],operations:Array[Long]) : Array[Complex] = {
    //return as an array
    if(degree == 1){
      Array(polynomial(0))
    }
    else{
      //get the roots of unity for the size of array
      val roots = Util.rootsOfUnity(degree)
      //partitioning
      var even_partition = new ArrayBuffer[Complex]()
      var odd_partition = new ArrayBuffer[Complex]()
      //even indices
      for(i <- 0 until polynomial.length by 2){
        even_partition += polynomial(i)
      }
      //odd indicies
      for(i <- 1 until polynomial.length by 2){
        odd_partition += polynomial(i)
      }
      //call recursive fft on each partition
      val e = fft(even_partition.length, even_partition.toArray,operations)
      val d = fft(odd_partition.length, odd_partition.toArray,operations)
      var result = new Array[Complex](degree)
      //combine the results from previous calls
      //left indices is kth even + kth root * kth odd
      //right indices is kth even - kth root * kth odd
      for(k <- 0 to ((degree/2)-1)){
        //account for both multiplies
        operations(3) = operations(3) + 2
        result(k) = e(k) + roots(k) * d(k)
        result(k+(degree/2)) = e(k) - roots(k) * d(k)

      }
      result
    }

  }


}

object AlgorithmTest {
  def outputOptions() = {
    println("1. Generate Random Polynomial")
    println("2. Read Polynomial From Input File")
    println("3. Write The Coefficients of Polynomial to Output File")
    println("4. Run Naive Algorithm for Polynomial Evaluation")
    println("5. Run Horner's Algorithm for Polynomial Evaluation")
    println("6. Run Decrease-By-Half Algorithm for Polynomial Evaluation")
    println("7. Run FFT Algorithm for Polynomial Evaluation")
    println("8. Display Running Times")
    println("9. Display Complex Number Multiplications")
    println("10. Exit")
  }
  def main(args: Array[String]): Unit = {
    var continue = true
    var input = 0
    var fileName = ""
    var polynomial : Array[Complex] = new Array[Complex](20)
    var degree = 0
    var operations = new Array[Long](4)
    var time = new Array[Double](4)
    var ranAll = Array.fill(4)(false)
    val naiveIndex = 0
    val hornersIndex = 1
    val decreaseByHalfIndex = 2
    val fftIndex = 3

    while(continue) {
      outputOptions();
      print("Choice: ")
      input =  readInt()
      input match {
        case 1 =>{
          ranAll = Array.fill(4)(false)
          var size = -1

          print("Input Size: ")
          size = readInt()
          while(size < 0){
            println("Invalid input! Must be positive ")
            print("Input Size: ")
            size = readInt()
          }
          degree = size
          polynomial = Util.generateRandomPolynomial(size)
          print("Polynomial: ")
          polynomial.foreach{ (x) => {print(x + " ")} }
          println()

        }
        case 2 => {
          //reset 
          ranAll = Array.fill(4)(false)
          fileName = readLine("Input File: ")
          val x = Util.readFromFile(fileName)
          if(x.isDefined){
            polynomial = x.get
            print("Polynomial: ")
            polynomial.foreach{ (x) => {print(x + " ") }}
            degree = polynomial.length
            println()
          }
          else {
            println("Failed to use file: Check format")
          }

 
        }
        case 3 => {
          fileName = readLine("Output File: ")
          Util.writeToFile(fileName,degree,polynomial)
        }
        case 4 => {
          println("Running Naive Algorithm")
          if(degree<= 0){
            println("Must initalize polynomial")
          }
          else{
            ranAll(naiveIndex) = true
            operations(naiveIndex) = 0
            val start = System.nanoTime //*1000 to seconds
            val result = Algorithms.runNaiveAlgorithm(degree,polynomial,operations)
            val end = System.nanoTime //*1000 to seconds
            time(naiveIndex) = (end - start)/1000
            println("Result: " + result.mkString(" "))
          }
        }
        case 5 => {
          println("Running Horner's Algoirthm")
          if(degree<= 0){
            println("Must initalize polynomial")
          }

          else{
            ranAll(hornersIndex) = true
            operations(hornersIndex) = 0
            val start = System.nanoTime //*1000 to seconds
            val result = Algorithms.hornersMethod(degree,polynomial,operations)
            val end = System.nanoTime //*1000 to seconds
            time(hornersIndex) = (end - start)/1000
            println("Result: " + result.mkString(" "))
          }
          //hornersAlgorithm()

        }
        case 6 => {
          println("Running Decrease-By-Half Algorithm")

          if(degree<= 0){
            println("Must initalize polynomial")
          }
          else{
            ranAll(decreaseByHalfIndex) = true
            operations(decreaseByHalfIndex) = 0
            val start = System.nanoTime //*1000 to seconds
            val result = Algorithms.decreaseByHalf(degree,polynomial,operations)
            val end = System.nanoTime //*1000 to seconds
            time(decreaseByHalfIndex) = (end - start)/1000
            println("Result: " + result.mkString(" "))
          }
          //decreaseByHalf()
        }
        case 7 => {
          println("Running FFT")

          if(degree<= 0){
            println("Must initalize polynomial")
          }
          else{
            ranAll(fftIndex) = true
            operations(fftIndex) = 0
            val start = System.nanoTime //*1000 to seconds
            val result = Algorithms.fft(degree,polynomial,operations)
            val end = System.nanoTime //*1000 to seconds
            time(fftIndex) = (end - start)/1000 // microseconds
            println("Result: " + result.mkString(" "))

          }
          //FFT()
        }
        case 8 => {
          println("Running Times")
          if(ranAll.foldLeft(true)( (x,y) => (x && y))){
            println("Naive Algorithm                  : " + time(0))
            println("Horner's Method Algorithm        : " + time(1))
            println("Decrease-By-Half Algorithm       : " + time(2))
            println("Fast Fourier Transform Algorithm : " + time(3))


          }
          //displayRunningTimes
        }
        case 9 => {
          println("Complex Number Multiplications")
          if(ranAll.foldLeft(true)( (x,y) => (x && y))){

            
    var time = Array.fill(4)(0)
            println("Naive " + operations(0))
            println("Horner's " + operations(1))
            println("Decrease by half " + operations(2))
            println("Fast Fourier Transform " + operations(3))


          }

        }
        case 10 => {
          println("Program Exiting")
          continue = false
        }
      }
    }
  }
}
