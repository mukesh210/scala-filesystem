package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.files.Directory

/*
  output: Result of previous command
 */
class State(val root: Directory, val wd: Directory, val output: String) {

  def setMessage(message: String): State = State(root, wd, message)

  def show: Unit = {
    println(output)
    print(State.SHELL_TOKEN)
  }
}

object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = ""): State = new State(root, wd, output)

}