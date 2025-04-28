# Welcome to robin-hood-profit

This is a reimplementation of
[robin-hood-profit](https://github.com/yaitskov/robin-hood-profit)
without pandas or similar library.

Python version struggles form pandas warnings I was not able to fix.

Main purpose for this version is to use
[attoparsec-monoidal](https://github.com/yaitskov/attoparsec-monoidal)
to implement right-to-left incremental CSV parser.  Such trick gives
advantage over RAM gready Python version loading whole CSV report in
memory.

Problem with these CSV reports that row order is important and rows
follow in reverse chronological order. So one way stream parsing with
original attoparsec is not possible.

Another minor feature is experimenting with Decimal type for lossless
handling accound balance and average stock price.

## Usage


```shell
$ nix-shell
$ cabal run rhprofit -- -p 'q1/2025 2025 *' -b 7000000 data

Instrument| Period|      Gain|  Dividend|    Profit|  Oversell
        BP|Q1/2025|        61|         0|        61|
        BP|   2025|        61|         0|        61|
        BP|      *|        61|         0|        61|
         F|Q1/2025|       155|         0|       155|
         F|   2025|       155|         0|       155|
         F|      *|       155|         0|       155|
      NVDA|Q1/2025|      1443|         0|      1443|
      NVDA|   2025|      1443|         0|      1443|
      NVDA|      *|      1443|         0|      1443|
       PBR|Q1/2025|         0|         0|         0|2025-02-03
       PBR|   2025|         0|         0|         0|2025-02-03
       PBR|      *|         0|         0|         0|2025-02-03
      SBUX|Q1/2025|         5|         0|         5|
      SBUX|   2025|         5|         0|         5|
      SBUX|      *|         5|         0|         5|
       SPY|Q1/2025|       762|         0|       762|2025-02-05
       SPY|   2025|       762|         0|       762|2025-02-05
       SPY|      *|       762|         0|       762|2025-02-05
       XOM|Q1/2025|       282|         0|       282|2025-02-04
       XOM|   2025|       282|         0|       282|2025-02-04
       XOM|      *|       282|         0|       282|2025-02-04
--------------------------------------------------------------
 Period|      Gain|  Interest|  Dividend|    Profit|Tax Prepay|  Oversell
Q1/2025|      2708|         0|         0|      2708|    649.92|2025-02-03
   2025|      2708|         0|         0|      2708|    649.92|2025-02-03
      *|      2708|         0|         0|      2708|    649.92|2025-02-03
```
