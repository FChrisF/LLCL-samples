Sample projects for the Light LCL (LLCL)
========================================


  Various Free Pascal/Lazarus and Delphi sample projects, to
compile with the Free Pascal/Lazarus Light LCL (LLCL): [Light LCL (LLCL)](https://github.com/FChrisF/LLCL)

  All the samples are distributed under the terms of the MIT
license: see LICENSE.txt

  To use them with the LLCL, add the corresponding path for
the LLCL files into these project options before compiling
them (see the README.txt file of the LLCL for more details).

  Projects are delivered for both types: Delphi (Delphi 7)
and Free Pascal/Lazarus.

  Unless notified specifically, all the samples can be
compiled using the LLCL or the standard LCL/VCL (see for each
sample in README.txt).


## Using only the LLCL


### CompFiles

Compare and search for identical files (same size and same
content) between 2 directories, or inside 1 directory.

[![CompFiles screenshot](https://FChrisF.github.io/LLCL-samples/captures/compfiles-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/compfiles-screen.png)
[![CompFiles results](https://FChrisF.github.io/LLCL-samples/captures/compfiles-results-th.png)](https://FChrisF.github.io/LLCL-samples/captures/compfiles-results.png)


### FileCRC

CRC32 and hash computes (MD5/SHA-1) for a file chosen by the
user.

[![FileCRC screenshot](https://FChrisF.github.io/LLCL-samples/captures/filecrc-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/filecrc-screen.png)
[![FileCRC fileselect](https://FChrisF.github.io/LLCL-samples/captures/filecrc-fileselect-th.png)](https://FChrisF.github.io/LLCL-samples/captures/filecrc-fileselect.png)


### MiniCalc

A very simple integer calculator.

[![MiniCalc screenshot](https://FChrisF.github.io/LLCL-samples/captures/minicalc-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/minicalc-screen.png)


## Using the LLCL and the Windows APIs


### PePing

Periodic ping for a given URL (Systray using).

[![PePing screenshot](https://FChrisF.github.io/LLCL-samples/captures/peping-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/peping-screen.png)
[![PePing trayicon](https://FChrisF.github.io/LLCL-samples/captures/peping-trayico-th.png)](https://FChrisF.github.io/LLCL-samples/captures/peping-trayico.png)


### Visual

Using GDI and OpenGL APIs.

[![Visual screenshot 1](https://FChrisF.github.io/LLCL-samples/captures/visual1-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/visual1-screen.png)

[![Visual screenshot 2](https://FChrisF.github.io/LLCL-samples/captures/visual2-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/visual2-screen.png)

[![Visual screenshot 3](https://FChrisF.github.io/LLCL-samples/captures/visual3-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/visual3-screen.png)


## Using a modified version of the LLCL


### GetWebPage

HTTP Get/Post using Indy. This sample requires a modified
version of the LLCL (a simple program to modify it is provided
with this sample).

[![GetWebPage screenshot](https://FChrisF.github.io/LLCL-samples/captures/getwebpage-screen-th.png)](https://FChrisF.github.io/LLCL-samples/captures/getwebpage-screen.png)


## Comparison of executable sizes

Comparison of executable sizes between the standard (LCL/VCL)
and the LLCL versions for the different sample projects

  Executables have been build with the same conditions in each
cases: architecture (32 bits), debugging (no debugging),
optimization level, icon files, FPC version ...


|Project          | LCL (Lazarus 1.4.4) | LLCL Free Pascal | VCL (Delphi7) | LLCL Delphi |
|:----------------|:-------------------:|:----------------:|:-------------:|:-----------:|
|Demo LLCLTest(1) |       1721 Kb       |      141 Kb      |    464 Kb     |    89 Kb    |
|CompFiles        |       1888 Kb       |      163 Kb      |    465 Kb     |   101 Kb    |
|FileCRC          |       1696 Kb       |      141 Kb      |    402 Kb     |    90 Kb    |
|MiniCalc         |       1652 Kb       |      118 Kb      |    379 Kb     |    75 Kb    |
|PePing           |       1699 Kb       |      141 Kb      |     (3)       |    92 Kb    |
|Visual(4)        |       1707 Kb       |      212 Kb      |    404 Kb     |   106 Kb    |
|GetWebPage(2)    |       2237 Kb       |      813 Kb      |    830 Kb     |   590 Kb    |

|Project          | LCL (Lazarus 1.6.0) | LLCL Free Pascal |
|:----------------|:-------------------:|:----------------:|
|Demo LLCLTest(1) |       1726 Kb       |      129 Kb      |
|CompFiles        |       1969 Kb       |      149 Kb      |
|FileCRC          |       1702 Kb       |      129 Kb      |
|MiniCalc         |       1662 Kb       |      108 Kb      |
|PePing           |       1709 Kb       |      129 Kb      |
|Visual(4)        |       1716 Kb       |      207 Kb      |
|GetWebPage(2)    |       2077 Kb       |      776 Kb      |


Notes:

1. demonstration project included in the LLCL: [LLCL](https://github.com/FChrisF/LLCL)
2. due to the size of the Indy library itself, the ratio of
   the different executable sizes is bigger than usual
3. because of the TTrayIcon component, this sample can't be
   compiled with the standard VCL with the old versions of
   Delphi (i.e. before Delphi 2006)
4. PNG images support included into the LLCL Delphi version,
   while not into the standard VCL one.
