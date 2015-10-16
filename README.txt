
            Sample projects for the
          LLCL - FPC/Lazarus Light LCL
          ----------------------------


  Various Free Pascal/Lazarus and Delphi sample projects, to
compile with the Free Pascal/Lazarus Light LCL (LLCL).

  All the samples are distributed under the terms of the MIT
license: see LICENSE.txt

  To use them with the LLCL, add the corresponding path for
the LLCL files into these project options before compiling
them (see the README.txt file of the LLCL for more details).

  Projects are delivered for both types: Delphi (Delphi 7)
and Free Pascal/Lazarus.

  Unless notified specifically, all the samples can be
compiled using the LLCL or the standard LCL/VCL.


Using only the LLCL
===================


FileCRC
-------
CRC32 and hash computes (MD5/SHA-1) for a file chosen by the
user: basic and dialog controls.
. conditional option to use a thread (to allow the user to
  cancel the computes),
. for Free Pascal, conditional option to use the "standard"
  Free Pascal CRC and hash functions, instead of those of
  this sample.


MiniCalc
--------
A very simple integer calculator: basic controls.
. sample to detect the LLCL and to use conditional code in
  this case (not recommended generally speaking).


Using the LLCL and the Windows APIs
===================================


PePing
------
Periodic ping for a given URL: basic controls, popup menu and
SysTray.
. uses Windows APIs to read data from a .ini file, to load
  various icon files, to translate the given URL to an IP
  address and to process the ping,
. for Free Pascal, requires Lazarus 1.2+ if compiled with the
  standard LCL (bug for icon changes in TTrayIcon component),
. as old versions of Delphi don't have a TTrayIcon component,
  2 types of form/unit are present for the Delphi project: one
  with the TTrayIcon component in the form, and another one
  without it (the TTrayIcon component is created dynamically
  in this case). So, this sample can't be compiled with the
  standard VCL with the old versions of Delphi (i.e. before
  Delphi 2006).


Visual
------
Using GDI and OpenGL APIs: basic controls and image. An
illustration of interactions between the LLCL and the Windows
GDI APIs, and the few differences with the standard LCL/VCL.
. uses GDI Windows APIs and GL/OpenGL APIs,
. for Free Pascal, the OpenGL interface requires the standard
  SysUtils unit. So, the SysUtils unit coming from the LLCL
  must be deleted or renamed before the compilation.


Using a modified version of the LLCL (standard Classes unit)
============================================================


GetWebPage
----------
HTTP Get/Post using Indy: workaround if the standard (i.e.
Free Pascal or Delphi) Classes unit is required for a package.
As Indy requires the standard Classes unit, a modified version
of the LLCL must be used.
. in the modified version of the LLCL, the LLCL 'Classes' unit
  is renamed in 'LCLasses' unit, and the other LLCL units are
  then using it. A simple program to modify the LLCL is
  provided with this sample: LClassesMake. It's a console
  program that has to be run in the directory containing the
  LLCL units to modify,
. the size of the final executable if still big, because of
  the intrinsic Indy library size,
. Indy requires the standard SysUtils unit. So, the SysUtils
  unit coming from the LLCL must be deleted or renamed before
  the compilation. As for the Variants unit for Delphi,
. requires the OpenSSL .dll files (libeay32.dll, ssleay32.dll)
  for HTTPS URLs,
. for Free Pascal, requires the zlib library (zlib1.dll) to
  activate compression,
. uses Windows APIs for cursor manipulations.
