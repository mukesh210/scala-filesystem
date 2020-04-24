package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {
    /*
    if no args, return same state
    else if just one arg, print to console
    else if multiple args
    {
      operator = next to last argument
      if >, echo to a file(may create file if not there)
      if >>, append to a file
      else just echo everything to console
    }
     */

    if(args.isEmpty) state
    else if(args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val fileName = args(args.length - 1)
      val content = createContent(args, args.length - 2)

      if(">>".equals(operator)) doEcho(state, content, fileName, append = true)
      else if(">".equals(operator)) doEcho(state, content, fileName, append = false)
      else state.setMessage(createContent(args, args.length))
    }
  }

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if(path.isEmpty) currentDirectory
    else if(path.tail.isEmpty) {
      val dirEntry: DirEntry = currentDirectory.findEntry(path.head)
      if(dirEntry == null)
        currentDirectory.addEntry(new File(currentDirectory.path, path.head, contents))
      else if(dirEntry.isDirectory) currentDirectory
      else if(append) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
      else currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val nextDirectory = currentDirectory.findEntry(path.head).asDirectory

      val newNextDirectory = getRootAfterEcho(nextDirectory, path.tail, contents, append)

      if(nextDirectory == newNextDirectory) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDirectory)
    }
  }

  def doEcho(state: State, content: String, fileName: String, append: Boolean): State = {
    if(fileName.contains(Directory.SEPARATOR))
      state.setMessage("echo: filename must not contain separator")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ fileName, content, append)
      if(newRoot == state.root)
        state.setMessage(s"$fileName: no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }

  // topIndex non-inclusive
  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, acc: String) : String= {
      if(currentIndex >= topIndex) acc
      else createContentHelper(currentIndex + 1, acc + " " + args(currentIndex))
    }

    createContentHelper(0, "")
  }
}
