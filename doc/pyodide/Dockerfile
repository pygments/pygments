# Dockerfile for building Pyodide with a Pygmenets version from the current checkout.
# For an example of how to use this image, see the `pyodide` target in the documentation's Makefile.
FROM ghcr.io/pyodide/pyodide:0.20.0 AS build-stage

WORKDIR pyodide

# Copy new meta with path to local Pygments instead of pypi url.
COPY doc/pyodide/meta.yaml packages/Pygments/

COPY . /pygments

# Add Pygments to the Pyodide build.
ENV PYODIDE_PACKAGES=Pygments

# Build Pyodide.
RUN make

FROM scratch AS export-stage

COPY --from=build-stage /src/pyodide/build /
