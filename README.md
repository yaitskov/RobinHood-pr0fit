# Robin-Hood quarter profit report builder

This is a Haskell reimplementation of
[robin-hood-profit](https://github.com/yaitskov/robin-hood-profit)
without pandas or similar data frame library.

The tool has a command line interface. It generates quarter profit
reports out of [Robin-Hood](https://robinhood.com/) activity reports
available in the CSV format. Quatities from such reports are important
for estimating quarter prepayment to [IRS](https://irs.gov/) during
current tax year.  According to IRS guidline if you postope pay till
next tax year (i.e Jan-Apr) and amount you owe is greater than $1k
you may have to pay a penalty.

In general any quarter report requires sequential processing of all
history (shares might be bought at one year and sold a decade later)
since a Robin-Hood account establishment preceding the target
quater. This data aspect accents algorithm implementation on memory
efficiency and throughput.



A Python version struggles with warnings from
[Pandas](https://pandas.pydata.org/) I was not able to fix.

Main purpose for this version is to use
[attoparsec-isotropic](https://github.com/yaitskov/attoparsec-isotropic)
to implement right-to-left incremental CSV parser.  Such trick gives
advantage over RAM gready Python version loading a whole CSV report in
memory.

A problem with these CSV reports that row order is important and rows
follow in the reverse chronological order. So one way stream parsing
with the original attoparsec is not possible.

Another minor feature is experimenting with Decimal type for lossless
handling account balance and average stock price.

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


## Static linking

Static is not enabled by default, because GitHub CI job times out.

```shell
nix-build --cores 20 -j 20 --arg staticBuild true
```
