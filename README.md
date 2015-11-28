# osm2gmap

osm2gmap generates maps for Garmin devices and Basecamp with a single
command. I created this program to make it easier to generate fresh
maps of a single country, without having to think every time about
what data sources need to be updated, and how to set options for
required dependencies.

Currently osm2gmap works to generate maps for single countries or
regions, where maps are available for download from [Geofabrik data
extracts](http://download.geofabrik.de/).

osm2gmap uses splitter, mkgmap, and gmapi-builder to generate
maps. osm2gmap automatically downloads and installs these
dependencies, as well as the most recent sea and bounds files.

## Systems Supported

This command-line program should work on Mac OS X and Linux operating
systems. Windows support may be added in the future.

## Installation

This project can be easily run using stack. To install Stack,
[follow the instructions for your platform](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

Then, change directory to the path where you cloned the project, and
execute the following commands:

```
stack build
MAP_COUNTRY=ecuador MAP_REGION=south-america stack exec
```

This should generate maps for your Garmin device and Basecamp for
Ecuador. It will place them in `~/.osm2gmap/output`.

## Customizing osm2gmap

To have osm2gmap build maps for another country, customize the
following two environment variables, based on the file that you'd like
to use from Geofabrik:

* `MAP_REGION`
* `MAP_COUNTRY`

For both, you should follow the hyphenated form that is in the file
you'd download from Geofabrik. For example, if the file to download
from Geofabrik is at the URL
http://download.geofabrik.de/south-america/ecuador-latest.osm.pbf, the
region would be `south-america` and the country `ecuador`.

## Author

Justin Leitgeb, <justin@stackbuilders.com>.

## Licence

MIT, see the [LICENSE](LICENSE) file.
