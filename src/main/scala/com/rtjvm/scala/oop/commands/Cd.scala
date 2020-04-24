package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State
import com.sun.tools.javac.parser.Tokens

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
  /*
  cd /something/somethingElse
  cd a/b/c = relative to current working directory
   */
  override def apply(state: State): State = {

    // 1. find root
    val root = state.root

    // 2. find the absolute path of the directory i want to cd to
    val wd: Directory = state.wd
    val absolutePath =
      if(dir.startsWith(Directory.SEPARATOR)) dir
      else if(wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir


    // 3. find the directory to cd to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)


    // 4. change the state given the directory
    if(destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(s"$dir : no such directory")
    else State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if(path.isEmpty || path.head.isEmpty) currentDirectory
      else if(path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir: DirEntry = currentDirectory.findEntry(path.head)
        if(nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if(path.isEmpty) result
      else if(path.head.equals(".")) collapseRelativeTokens(path.tail, result)
      else if(path.head.equals("..")) {
        if(result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      }
      else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    // 1. find tokens in path
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // 1.5 eliminate/collapse relative tokens
    /*
    ["a", "."] => ["a"](ignore .)

    ["a", ".."] => [](element before .. will disappear)
     */
    val newTokens: List[String] = collapseRelativeTokens(tokens, List())
    // 2. navigate to correct entry
    if(newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
