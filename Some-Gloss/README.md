This project is an introduction to Haskell IO and the Gloss visualization library.

The purpose is to create choropleth maps by mashing up two sources of data: geographic boundary data and election return data.

All voting and geographic data files can be found in the .zip file.

The menu system allows the user to visualize different regions in the United States and color them according to their voting patterns. Red for Republican and Blue for Democrats.

In addition to this, the program allows for a more nuanced visualization that reveals that the United States is not as polarized by geography as suggested by the above representation (Red vs. Blue). This corresponds to the [Purple America](https://en.wikipedia.org/wiki/Purple_America) option (-p) in the program.

You must use the ghc compiler to run this program.

Usage:

./Main region [-w | -rgb year | -p year]

if no flag specified, -w used by default.

-w: produces a wireframe map (i.e., an outline map) for a region.

-rgb year : produces a Red or Blue map for the region and year.

-p year : produces a purple map for the region and year.
