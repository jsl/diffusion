# osm2gmap

osm2gmap generates maps for Garmin devices and Basecamp with a single
command. I created this program to make it easier to generate fresh
maps of a single country, without having to think every time about
what data sources need to be updated, and how to set options for
required dependencies.

Currently osm2gmap works to generate maps for single countries or
regions, where maps are available for download from [Geofabrik data
extracts](http://download.geofabrik.de/).

osm2gmap uses [splitter](http://www.mkgmap.org.uk/doc/splitter.html),
[mkgmap](http://www.mkgmap.org.uk/doc/index.html), and
[gmapi-builder](http://wiki.openstreetmap.org/wiki/Gmapibuilder) to
generate maps. osm2gmap automatically downloads and installs these
dependencies, as well as the most recent sea and bounds
files. osm2gmap installs all dependencies locally, and does not
require root privileges.

## Systems Supported

This command-line program should work on Mac OS X and Linux operating
systems. Windows support may be added in the future.

## Installation

This project can be easily run using stack. To install Stack,
[follow the instructions for your platform](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

(Download the zip archive of this
project)[https://github.com/jsl/osm2gmap/archive/master.zip] and
unpack the archive. Open a terminal and change your directory to the
directory where the project was unpacked. Then, build the project with
`stack build`.

# Running osm2gmap

Tell osm2gmap which region and country to build maps for by
setting the command-line optiodns, `region` and `country`.

For both `region` and `country`, you should follow the
hyphenated form that is present in the URL of the file you wish to use
from [Geofabrik](http://download.geofabrik.de/). For example, if the
file to download from Geofabrik is at the URL
http://download.geofabrik.de/south-america/ecuador-latest.osm.pbf, the
region would be `south-america` and the country `ecuador`.

The following command will build maps for Ecuador:

```
stack exec -- osm2gmap --region south-america --country ecuador
```

After running this command, the resulting files will be placed in
`~/.osm2gmap/output`.

# Forcing Usage of cached Bounds and Sea Files

Since the "bounds" and "sea" files are large, you can skip checking
for updates for these files by using the `--cached-bounds` and
`--cached-sea` options, respectively.

## Author

Justin Leitgeb, <justin@stackbuilders.com>.

## Licence

MIT, see the [LICENSE](LICENSE) file.
