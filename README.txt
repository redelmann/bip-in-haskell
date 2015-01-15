
This file contains information about how to install the Haskell BIP framework
and how to run example programs.

=== Installation ===

To install Haskell and the Haskell framework, please follow the following
step by step instructions.


--- Copying the directory to the hard drive ---

Before we can actually install Haskell and the Haskell BIP framework,
please ensure that the directory containing this README file has been
copied to your local hard drive. Some of the commands must be issued
from this directory.


--- Installing Haskell Platform ---

To install the framework and run examples, the Haskell platform is required.
To obtain and install the Haskell platform, simply visit:
    
    https://www.haskell.org/platform

The platform comes with a Haskell compiler (ghc), an interpreter (ghci)
and a build tool (cabal).


--- Upgrading the build tool ---

Once the Haskell platform has been downloaded and installed, 
the build tool (cabal) must be upgraded.
To do so, simply enter the following command in a terminal:

    cabal install cabal-install

This ensures that the latest version of the build tool is installed.


--- Installing library dependencies ---

The Haskell framework makes use of some third party libraries.
In order for the framework to be usable, those libraries must be installed.

From the command line, instruct the cabal tool (just installed with the
Haskell platform) to download and install those packages.

    cabal install criterion operational profunctors random-shuffle

Note that the installation of those packages may take some time.


--- Installing the Haskell framework ---

Once the Haskell platform and the required packages have been installed,
the Haskell BIP framework can simply be installed using the following two
commands, ensuring that they are issued from this local directory. 

    cabal configure
    cabal install

Once those two commands have been executed, the Haskell BIP framework is
actually installed on the system and can be imported by any Haskell program.


=== Running the examples ===

To start the examples showed in the thesis, you can issue the following
commands from this directory. Please make sure that the Haskell BIP framework
has previously been installed.

For the token ring example, please run:

    cabal run tokenring

For the producers consumers, please run:

    cabal run producersconsumers

(This example doesn't produce any output)

For the dining philosophers example, run:

    cabal run diningphilosophers

