# OSM Build Tools

Diffusion is a collection of build tools for OSM maps. Specifically, diffusion
can help to download planet country, break maps up into local maps, and generate
end-user maps for devices such as Garmin.

## Installation

This project can be easily run using stack. To install Stack,
[follow the instructions for your platform](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

## Usage

### Generating an Ecuadorian map from the Planet

```
stack build
stack exec -- dotenv -f conf/genregionmap/ecuador-from-latam.dotenv genregionmap
```

## Author

Justin Leitgeb, <justin@stackbuilders.com>.

## License

MIT, see the [LICENSE](LICENSE) file.
