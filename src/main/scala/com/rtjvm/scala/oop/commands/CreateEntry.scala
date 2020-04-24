package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(entryName: String) extends Command {
  override def apply(state: State): State = {
    val wd: Directory = state.wd
    if(wd.hasEntry(entryName))
      state.setMessage(s"Entry ${entryName} already exists")
    else if(entryName.contains(Directory.SEPARATOR))
      state.setMessage(s"${entryName} must not contain separator")
    else if(checkIllegal(entryName))
      state.setMessage(s"${entryName}: Illegal entry name!")
    else {
      doCreateEntry(state, entryName)
    }
  }

  def doCreateEntry(state: State, str: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry: Directory = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd: Directory = state.wd

    // 1. all the directories in full path
    val allDirsInPath: List[String] = wd.getAllFoldersInPath

    // 2. create new directory entry in wd
    // TODO implement this
    val newEntry: DirEntry = createSpecificEntry(state)

    // 3. update the whole directory structure starting from root
    // Directory structure is IMMUTABLE
    val newRoot: Directory = updateStructure(state.root, allDirsInPath, newEntry)

    // 4. find new working directory INSTANCE given wd's full path, in the new directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def checkIllegal(str: String): Boolean = entryName.contains(".")

  def createSpecificEntry(state: State): DirEntry
}
