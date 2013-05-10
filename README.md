<a name="Cheaterpress" />Cheaterpress
============

This is a small program I put together as my first "real" Scala application. It's effectively an anagram finder with letter prioritization, written in functional style in Scala.

<a name="Usage" />Usage
=====
You should be able to `sbt run` directly from inside the Cheaterpress directory
Game files are described in [Game Files](#GameFiles), and can be used in the following way:

    sbt run games/MyFirstGame.txt

... will give stats on how many words of each length are left in the game

    sbt run --mode cheat --priority planets games/MyFirstGame.txt

... will print out possible words (excluding ones that are in the [exclude list](#ExcludeList)), prioritizing the letters in "planets" (good for reclaiming tiles in Letterpress)

    sbt run -h

... will print out a brief overview of options.

<a name="GameFiles" />Game Files
==========
Game files are plain text files with the following structure:

 * The first line is all available letters, (in Letterpress, this is typically the game board itself)
 * <a name="ExcludeList" />The rest of the lines are words to exclude from results and stats, (in Letterpress, this is typically words that have already been played)
