package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // 1. get working dir
    val wd = state.wd

    // 2. get absolute path
    val absolutePath =
      if(name.startsWith(Directory.SEPARATOR)) name
      else if(wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // 3. do some checks
    if(Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear war not supported yet!")
    else
      doRm(state, absolutePath)
  }

  // 4. find the entry to remove
  // 5. update the structure like we do for MKDIR
  def doRm(state: State, path: String): State = {

    // TODO remember to implement findDescendant in Directory
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if(path.isEmpty) currentDirectory // rm command failed
      else if(path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if(!nextDir.isDirectory) currentDirectory
        else {
          val newNextDirectory = rmHelper(nextDir.asDirectory, path.tail)
          if(newNextDirectory == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDirectory)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory= rmHelper(state.root, tokens)
    if(newRoot == state.root)
      state.setMessage(s"${path}: No such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
