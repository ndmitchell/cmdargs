# CmdArgs: Easy Command Line Processing [![Hackage version](https://img.shields.io/hackage/v/cmdargs.svg?style=flat)](https://hackage.haskell.org/package/cmdargs) [![Build Status](https://img.shields.io/travis/ndmitchell/cmdargs.svg?style=flat)](https://travis-ci.org/ndmitchell/cmdargs)

<p>
	CmdArgs is a Haskell library for defining command line parsers. The two features that make it a better choice than the standard <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/System-Console-GetOpt.html">getopt library</a> are:
</p>
<ol>
	<li>It's very concise to use. The HLint command line handling is three times shorter with CmdArgs.</li>
	<li>It supports programs with multiple modes, such as <a href="http://darcs.net">darcs</a> or <a href="http://haskell.org/cabal/">Cabal</a>.</li>
</ol>
<p>
	A very simple example of a command line processor is:
</p>
<pre>
data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)

sample = Sample{hello = def &amp;= help "World argument" &amp;= opt "world"}
         &amp;= summary "Sample v1"

main = print =&lt;&lt; cmdArgs sample
</pre>
<p>
	Despite being very concise, this processor is already fairly well featured:
</p>
<pre>
$ runhaskell Sample.hs --hello=world
Sample {hello = "world"}

$ runhaskell Sample.hs --help
Sample v1, (C) Neil Mitchell 2009

sample [FLAG]

  -? --help[=FORMAT]  Show usage information (optional format)
  -V --version        Show version information
  -v --verbose        Higher verbosity
  -q --quiet          Lower verbosity
  -h --hello=VALUE    World argument (default=world)
</pre>

<h2>User Manual</h2>

<p>
	The rest of this document explains how to write the "hello world" of command line processors, then how to extend it with features into a complex command line processor. Finally this document gives three samples, which the <tt>cmdargs</tt> program can run. The three samples are:
</p>
<ol>
    <li><tt>hlint</tt> - the <a href="http://community.haskell.org/~ndm/hlint/">HLint</a> program.</li>
    <li><tt>diffy</tt> - a program to compare the differences between directories.</li>
    <li><tt>maker</tt> - a make style program.</li>
</ol>
<p>
	For each example you are encouraged to look at it's source (see the <a href="http://community.haskell.org/~ndm/darcs/hlint">darcs repo</a>, or the bottom of this document) and run it (try <tt>cmdargs hlint --help</tt>). The HLint program is fairly standard in terms of it's argument processing, and previously used the <a href="http://haskell.org/ghc/docs/latest/html/libraries/base/System-Console-GetOpt.html">System.Console.GetOpt</a> library. Using GetOpt required 90 lines and a reasonable amount of duplication. Using CmdArgs the code requires 30 lines, and the logic is much simpler.
</p>
<h3>Acknowledgements</h3>
<p>
	Thanks to Kevin Quick for substantial patches, and additional code contributions from Sebastian Fischer and Daniel Schoepe.
</p>


<h2>Hello World Example</h2>
<p>
	The following code defines a complete command line argument processor:
</p>

    {-# LANGUAGE DeriveDataTypeable #-}
    module Sample where
    import System.Console.CmdArgs
    
    data Sample = Sample {hello :: String}
                  deriving (Show, Data, Typeable)
    
    sample = Sample{hello = def}
    
    main = print =<< cmdArgs sample

<p>
	To use the CmdArgs library there are three steps:
</p>
<ol>
	<li>Define a record data type (<tt>Sample</tt>) that contains a field for each argument. This type needs to have instances for <tt>Show</tt>, <tt>Data</tt> and <tt>Typeable</tt>.</li>
	<li>Give a value of that type (<tt>sample</tt>) with default values (<tt>def</tt> is a default value of any type, but I could also have written <tt>""</tt>). This value is turned into a command line by calling the <tt>cmdArgs</tt> function.</li>
</ol>
<p>
	Now we have a reasonably functional command line argument processor. Some sample interactions are:
</p>
<pre>
$ runhaskell Sample.hs --hello=world
Sample {hello = "world"}

$ runhaskell Sample.hs --version
The sample program

$ runhaskell Sample.hs --help
The sample program

sample [OPTIONS]

  -? --help        Display help message
  -V --version     Print version information
  -h --hello=ITEM
</pre>
<p>
	CmdArgs uses defaults to automatically infer a command line parser for a value, and provides annotations to override any of the the defaults. CmdArgs automatically supports <tt>--help</tt> and <tt>--version</tt> flags, and optionally supports verbosity flags.
</p>

<h2>Specifying Attributes</h2>
<p>
	In order to control the behaviour we can add attributes. For example to add an attribute specifying the help text for the <tt>--hello</tt> argument we can write:
</p>
<pre>
sample = Sample{hello = def &= help "Who to say hello to"}
</pre>
<p>
	We can add additional attributes, for example to specify the type of the value expected by hello:
</p>
<pre>
sample = Sample {hello = def &= help "Who to say hello to" &= typ "WORLD"}
</pre>
<p>
	Now when running <tt>--help</tt> the final line is:
</p>
<pre>
  -h --hello=WORLD  Who to say hello to
</pre>
<p>
	There are many more attributes, detailed in the <a href="http://hackage.haskell.org/packages/archive/cmdargs/latest/doc/html/System-Console-CmdArgs.html#2">Haddock documentation</a>.
</p>


<h2>Multiple Modes</h2>
<p>
	To specify a program with multiple modes, similar to <a href="http://darcs.net/">darcs</a>, we can supply a data type with multiple constructors, for example:
</p>

    
    data Sample = Hello {whom :: String}
                | Goodbye
                  deriving (Show, Data, Typeable)
    
    hello = Hello{whom = def}
    goodbye = Goodbye
    
    main = print =<< cmdArgs (modes [hello,goodbye])

<p>
	Compared to the first example, we now have multiple constructors, and a sample value for each constructor is passed to <tt>cmdArgs</tt>. Some sample interactions with this command line are:
</p>
<pre>
$ runhaskell Sample.hs hello --whom=world
Hello {whom = "world"}

$ runhaskell Sample.hs goodbye
Goodbye

$ runhaskell Sample.hs --help
The sample program

sample [OPTIONS]

 Common flags
  -? --help       Display help message
  -V --version    Print version information

sample hello [OPTIONS]

  -w --whom=ITEM

sample goodbye [OPTIONS]
</pre>
<p>
	As before, the behaviour can be customised using attributes.
</p>

<h2>Larger Examples</h2>

<p>
	For each of the following examples we first explain the purpose of the program, then give the source code, and finally the output of <tt>--help=HTML</tt>. The programs are intended to show sample uses of CmdArgs, and are available to experiment with through <tt>cmdargs <i>progname</i></tt>.
</p>

<h3>HLint</h3>

<p>
	The <a href="http://community.haskell.org/~ndm/hlint/">HLint</a> program analyses a list of files, using various options to control the analysis. The command line processing is simple, but a few interesting points are:
</p>
<ul>
	<li>The <tt>--report</tt> flag can be used to output a report in a standard location, but giving the flag a value changes where the file is output.</li>
	<li>The <tt>color</tt> field is assigned two flag aliases, <tt>--colour</tt> and <tt>-c</tt>. Assigning the <tt>-c</tt> short flag explicitly stops either of the CPP fields using it.</li>
	<li>The <tt>show_</tt> field would clash with <tt>show</tt> if given the expected name, but CmdArgs automatically strips the trailing underscore.</li>
	<li>The <tt>cpp_define</tt> field has an underscore in it's name, which is transformed into a hyphen for the flag name.</li>
</ul>

<!-- BEGIN code hlint -->
<pre>
{-# LANGUAGE DeriveDataTypeable #-}
module HLint where
import System.Console.CmdArgs

data HLint = HLint
    {report :: [FilePath]
    ,hint :: [FilePath]
    ,color :: Bool
    ,ignore_ :: [String]
    ,show_ :: Bool
    ,extension :: [String]
    ,language :: [String]
    ,utf8 :: Bool
    ,encoding :: String
    ,find :: [FilePath]
    ,test_ :: Bool
    ,datadir :: [FilePath]
    ,cpp_define :: [String]
    ,cpp_include :: [FilePath]
    ,files :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

hlint = HLint
    {report = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
    ,hint = def &= typFile &= help "Hint/ignore file to use"
    ,color = def &= name "c" &= name "colour" &= help "Color the output (requires ANSI terminal)"
    ,ignore_ = def &= typ "MESSAGE" &= help "Ignore a particular hint"
    ,show_ = def &= help "Show all ignored ideas"
    ,extension = def &= typ "EXT" &= help "File extensions to search (defaults to hs and lhs)"
    ,language = def &= name "X" &= typ "LANG" &= help "Language extension (Arrows, NoCPP)"
    ,utf8 = def &= help "Use UTF-8 text encoding"
    ,encoding = def &= typ "ENC" &= help "Choose the text encoding"
    ,find = def &= typFile &= help "Find hints in a Haskell file"
    ,test_ = def &= help "Run in test mode"
    ,datadir = def &= typDir &= help "Override the data directory"
    ,cpp_define = def &= typ "NAME[=VALUE]" &= help "CPP #define"
    ,cpp_include = def &= typDir &= help "CPP include path"
    ,files = def &= args &= typ "FILES/DIRS"
    } &=
    verbosity &=
    help "Suggest improvements to Haskell source code" &=
    summary "HLint v0.0.0, (C) Neil Mitchell" &=
    details ["Hlint gives hints on how to improve Haskell code",""
            ,"To check all Haskell files in 'src' and generate a report type:","  hlint src --report"]

mode = cmdArgsMode hlint
</pre>

    HLint v0.0.0, (C) Neil Mitchell
    
    hlint [OPTIONS] [FILES/DIRS]
    Suggest improvements to Haskell source code
    
    Common flags:
      -r --report[=FILE]	        Generate a report in HTML
      -h --hint=FILE	            Hint/ignore file to use
      -c --colour --color	        Color the output (requires ANSI terminal)
      -i --ignore=MESSAGE	        Ignore a particular hint
      -s --show                     Show all ignored ideas
         --extension=EXT            File extensions to search (defaults to hs and lhs)
      -X --language=LANG	        Language extension (Arrows, NoCPP)
      -u --utf8	                    Use UTF-8 text encoding
         --encoding=ENC	            Choose the text encoding
      -f --find=FILE	            Find hints in a Haskell file
      -t --test	                    Run in test mode
      -d --datadir=DIR	            Override the data directory
         --cpp-define=NAME[=VALUE]  CPP #define
         --cpp-include=DIR	        CPP include path
      -? --help	                    Display help message
      -V --version	                Print version information
      -v --verbose	                Loud verbosity
      -q --quiet	                Quiet verbosity
    
    Hlint gives hints on how to improve Haskell code
    
    To check all Haskell files in 'src' and generate a report type:
      hlint src --report
	

<h3>Diffy</h3>

<p>
	The Diffy sample is a based on the idea of creating directory listings and comparing them. The tool can operate in two separate modes, <tt>create</tt> or <tt>diff</tt>. This sample is fictional, but the ideas are drawn from a real program. A few notable features:
</p>
<ul>
	<li>There are multiple modes of execution, creating and diffing.</li>
	<li>The diff mode takes exactly two arguments, the old file and the new file.</li>
	<li>Default values are given for the <tt>out</tt> field, which are different in both modes.</li>
</ul>

<!-- BEGIN code diffy -->
<pre>
{-# LANGUAGE DeriveDataTypeable #-}
module Diffy where
import System.Console.CmdArgs

data Diffy = Create {src :: Maybe FilePath, out :: FilePath}
           | Diff {old :: FilePath, new :: FilePath, out :: FilePath}
             deriving (Data,Typeable,Show,Eq)

outFlags x = x &= help "Output file" &= typFile

create = Create
    {src = def &= help "Source directory" &= typDir
    ,out = outFlags "ls.txt"
    } &= help "Create a fingerprint"

diff = Diff
    {old = def &= typ "OLDFILE" &= argPos 0
    ,new = def &= typ "NEWFILE" &= argPos 1
    ,out = outFlags "diff.txt"
    } &= help "Perform a diff"

mode = cmdArgsMode $ modes [create,diff] &= help "Create and compare differences" &= program "diffy" &= summary "Diffy v1.0"
</pre>
<!-- END -->
<!-- BEGIN help diffy -->
	Diffy v1.0
	 
	diffy [COMMAND] ... [OPTIONS]
	  Create and compare differences
	 
	Common flags:
	  -o --out=FILE	 Output file
	  -? --help	     Display help message
	  -V --version	 Print version information
	 
	diffy create [OPTIONS]
	  Create a fingerprint
	 
	  -s  --src=DIR  Source directory
	 
	diffy diff [OPTIONS] OLDFILE NEWFILE
	  Perform a diff

<!-- END -->

<h3>Maker</h3>

<p>
	The Maker sample is based around a build system, where we can either build a project, clean the temporary files, or run a test. Some interesting features are:
</p>
<ul>
	<li>The build mode is the default, so <tt>maker</tt> on it's own will be interpretted as a build command.</li>
	<li>The build method is an enumeration.</li>
	<li>The <tt>threads</tt> field is in two of the constructors, but not all three. It is given the short flag <tt>-j</tt>, rather than the default <tt>-t</tt>.</li>
</ul>


<!-- BEGIN code maker -->
<pre>
{-# LANGUAGE DeriveDataTypeable #-}
module Maker where
import System.Console.CmdArgs

data Method = Debug | Release | Profile
              deriving (Data,Typeable,Show,Eq)

data Maker
    = Wipe
    | Test {threads :: Int, extra :: [String]}
    | Build {threads :: Int, method :: Method, files :: [FilePath]}
      deriving (Data,Typeable,Show,Eq)

threadsMsg x = x &= help "Number of threads to use" &= name "j" &= typ "NUM"

wipe = Wipe &= help "Clean all build objects"

test_ = Test
    {threads = threadsMsg def
    ,extra = def &= typ "ANY" &= args
    } &= help "Run the test suite"

build = Build
    {threads = threadsMsg def
    ,method = enum
        [Release &= help "Release build"
        ,Debug &= help "Debug build"
        ,Profile &= help "Profile build"]
    ,files = def &= args
    } &= help "Build the project" &= auto

mode = cmdArgsMode $ modes [build,wipe,test_] &= help "Build helper program" &= program "maker" &= summary "Maker v1.0\nMake it"
</pre>
<!-- END -->
<!-- BEGIN help maker -->
    Maker v1.0
      Make it
     
    maker [COMMAND] ... [OPTIONS]
      Build helper program
     
    Common flags:
      -? --help     Display help message
      -V --version  Print version information
     
    maker [build] [OPTIONS] [ITEM]
      Build the project
     
      -j --threads=NUM  Number of threads to use
      -r --release      Release build
      -d --debug        Debug build
      -p --profile      Profile build
     
    maker wipe [OPTIONS]
      Clean all build objects
     
    maker test [OPTIONS] [ANY]
      Run the test suite
     
      -j --threads=NUM  Number of threads to use
<!-- END -->
