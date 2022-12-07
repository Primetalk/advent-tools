package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Rfc5234.wsp
import cats.parse.Rfc5234
import cats.data.NonEmptyList

/**
  * https://adventofcode.com/2022/day/7
--- Day 7: No Space Left On Device ---

You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?

The device the Elves gave you has problems with more than just its communication system. You try to run a system update:

$ system-update --please --pretty-please-with-sugar-on-top
Error: No space left on device

Perhaps you can delete some files to make space for the update?

You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:

$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k

The filesystem consists of a tree of files (plain data) and directories (which can contain other directories or files). The outermost directory is called /. You can navigate around the filesystem, moving into or out of directories and listing the contents of the directory you're currently in.

Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:

    cd means change directory. This changes which directory is the current directory, but the specific result depends on the argument:
        cd x moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
        cd .. moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
        cd / switches the current directory to the outermost directory, /.
    ls means list. It prints out all of the files and directories immediately contained by the current directory:
        123 abc means that the current directory contains a file named abc with size 123.
        dir xyz means that the current directory contains a directory named xyz.

Given the commands and output in the example above, you can determine that the filesystem looks visually like this:

- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)

Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes.

Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the total size of each directory. The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)

The total sizes of the directories above can be found as follows:

    The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
    The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
    Directory d has total size 24933642.
    As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.

To begin, find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes. In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584). (As in this example, this process can count files more than once!)

Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?

Your puzzle answer was 2061777.
--- Part Two ---

Now, you're ready to choose a directory to delete.

The total disk space available to the filesystem is 70000000. To run the update, you need unused space of at least 30000000. You need to find a directory you can delete that will free up enough space to run the update.

In the example above, the total size of the outermost directory (and thus the total amount of used space) is 48381165; this means that the size of the unused space must currently be 21618835, which isn't quite the 30000000 required by the update. Therefore, the update still requires a directory with total size of at least 8381165 to be deleted before it can run.

To achieve this, you have the following options:

    Delete directory e, which would increase unused space by 584.
    Delete directory a, which would increase unused space by 94853.
    Delete directory d, which would increase unused space by 24933642.
    Delete directory /, which would increase unused space by 48381165.

Directories e and a are both too small; deleting them would not free up enough space. However, directories d and / are both big enough! Between these, choose the smallest: d, increasing unused space by 24933642.

Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. What is the total size of that directory?

Your puzzle answer was 4473403.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2207 extends Utils:

  import org.primetalk.advent3.tools.ParsingUtils._

  sealed trait InputLine
  sealed trait Command extends InputLine
  case class Cd(newDir: String) extends Command
  case object Ls extends Command
  sealed trait FileOrDir extends InputLine
  case class Dir(name: String) extends FileOrDir
  case class File(name: String, size: Int) extends FileOrDir
  
  val name = Parser.charWhere(ch => ch.isLetter || ch == '.' || ch == '/').rep.string
  val cd: Parser[Cd] = Parser.string("cd ") *> name.map(Cd.apply)
  val ls: Parser[Ls.type] = Parser.string("ls").as(Ls)
  val command: Parser[Command] = Parser.string("$ ").backtrack *> (cd | ls)
  val dir: Parser[Dir] = Parser.string("dir ").backtrack *> name.map(Dir.apply)
  val file: Parser[File] = ((positiveNumber <* Parser.char(' ')) ~ name).map((s,n) => File(n,s))
 
  val line = (command.backtrack | file.backtrack | dir.backtrack) <* eol
  
  given lines: Parser[List[InputLine]] = line.rep.map(_.toList)

  val inputLines = readThisObjectInput.parseUnsafe[List[InputLine]]

  type Path = List[String]

  sealed trait FileSystemObject:
    def size: Long

  case class Directory(contents: Map[String, FileSystemObject]) extends FileSystemObject:
    def size: Long = contents.values.map(_.size).sum

  case class File2(size: Long) extends FileSystemObject

  def get(dir: Directory, path: Path): FileSystemObject =
    path match
      case head :: next => 
        dir.contents.get(head) match
          case Some(dir2: Directory) =>
            get(dir2, next)
          case _ =>
            throw IllegalArgumentException(s"not found $path")
      case Nil => 
        dir

  def updated(dir: Directory, path: Path, value: FileSystemObject): Directory =
    path match
      case head :: tail =>
        val newValue = tail match
          case Nil => value
          case head2 :: _ =>
            val dir2 =
              dir.contents.get(head) match
                case Some(dir2: Directory) =>
                  dir2
                case Some(file) =>
                  throw IllegalArgumentException(s"found file $file at $path")
                case None =>
                  Directory(Map())
            updated(dir2, tail, value)
        dir.copy(contents = dir.contents.updated(head, newValue))
      case Nil =>
        throw IllegalArgumentException(s"merging? $dir with $value")

  def buildDirectoryStructure(reversePath: Path, rootSoFar: Directory, lines: List[InputLine]): Directory =
    lines match
      case line :: next => 
         line match
            case Cd("/") => 
              buildDirectoryStructure(List(), rootSoFar, next)
            case Cd("..") => 
              buildDirectoryStructure(reversePath.tail, rootSoFar, next)
            case Cd(name) => 
              buildDirectoryStructure(name :: reversePath, rootSoFar, next)
            case Ls =>
              buildDirectoryStructure(reversePath, rootSoFar, next)
            case Dir(name) =>
              buildDirectoryStructure(reversePath, 
                updated(rootSoFar, (name::reversePath).reverse, Directory(Map())), next)
            case File(name, size) =>
              buildDirectoryStructure(reversePath, 
                updated(rootSoFar, (name::reversePath).reverse, File2(size)), next)

      case Nil =>
        rootSoFar
    
  def calcSizeLess100000(toCheck: List[Directory], accum: Long = 0L): Long =
    toCheck match
      case dir :: tail =>
        val lst = dir.contents.values.collect{ case d: Directory => d}.toList
        if dir.size <= 100000 then
          calcSizeLess100000(lst ::: tail, accum + dir.size)
        else
          calcSizeLess100000(lst ::: tail, accum)
      case Nil => 
        accum

  val root = buildDirectoryStructure(List(), Directory(Map()), inputLines)
  // 1813136
  lazy val answer1: Long =     
    calcSizeLess100000(List(root))

  //Part 2

  def findAllSizes(toCheck: List[Directory], accum: List[Long] = Nil): List[Long] =
    toCheck match
      case dir :: tail =>
        val lst = dir.contents.values.collect{ case d: Directory => d}.toList
        findAllSizes(lst ::: tail, dir.size :: accum )
      case Nil => 
        accum


  lazy val answer2: Long = 
    val total = 70000000
    val need = 30000000
    val freeAtLeast = need  + root.size - total
    val sizes = findAllSizes(List(root))
    sizes.filter(_ >= freeAtLeast).min

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
