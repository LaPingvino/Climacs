Introduction
------------

Welcome to Climacs, a project to create a Common Lisp version of
Emacs.  In fact, this project is merely meant to replace the buffer
protocols of Goatee (the editor of McCLIM) and Portable Hemlock, in an
attempt to ultimately merge those two projects.  

Climacs contributes (or will contribute) a few important things that
are not found neither in Goatee nor in Portable Hemlock:

   * A buffer protocol with a potentially very efficient
     implementation based on flexichains;

   * An `undo' protocol that is both general and powerful and that
     works on top of the buffer protocol;

   * An implementation of the association between multi-keystroke
     gestures and commands using nested CLIM command tables;

   * The possibility of having the buffer contain arbitrary
     objects. These will be rendered by the CLIM `present' function,
     so that they become clickable in the right context;

   * Syntax highlighting in the form of incremental parsers, especially
     for Common Lisp code. 

How to contribute
-----------------

The official page of this project is/was http://common-lisp.net/project/climacs/ and
archives should likewise be accessible.  Contributions to make the framework and the editor a
nicer environment are very welcome.

The project website still refers to the CVS repository. That doesn't
exist any more and for a long time I thought the history was lost.
Turns out the actual history can be found on the common-lisp.net
gitlab and the information needs to be updated. I updated this 
repository on https://github.com/LaPingvino/Climacs to be up to date
with the official conversion of the CVS history at the GitLab instance
https://gitlab.common-lisp.net/climacs/climacs

What to work on
---------------

Climacs is a full CLIM application, and should take advantage of CLIM
as much as possible, in particular for completion, presentations, etc.

It is hard to resist the temptation to re-implement functionality that
is currently in Emacs but that really belongs either in CLIM or in
separate CLIM applications.  In particular, we are NOT interested in:

   * Mail and News readers (see mel and Hermes)

   * A debugger (see the debugger pane of McCLIM)

   * An inspector (see the inspector pane of McCLIM)

   * Dired, Bufed, Shell mode, Calendar and other functionality that
     is best done as a CLIM pane or a separate CLIM application

We ARE however interested in the following items:

   * A grammar checker based on an incremental parser for natural
     languages

   * Incremental parsers for programming languages other than Common
     Lisp

   * A kill ring (perhaps the one from Portable Hemlock will do)

   * Indentation warnings for Common Lisp

Installation
------------

To install Climacs, see the INSTALL file.
