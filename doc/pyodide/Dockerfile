# Dockerfile for building Pyodide with a Pygments version from the current checkout.
# This is used by the `pyodide` tox environment (see /tox.ini).
FROM ghcr.io/pygments/pyodide-base:0.28.1 AS build-stage

COPY . /pygments
RUN sed -i -e 's,dynamic =.*,version = "2.99",' /pygments/pyproject.toml

WORKDIR /pyodide
COPY doc/pyodide/meta.yaml packages/Pygments/
RUN cd /emsdk && . ./emsdk_env.sh && cd /pyodide && pyodide build-recipes Pygments --install
RUN for file in pyodide.js pyodide.asm.js pyodide.asm.wasm python_stdlib.zip; do cp /root/.cache/.pyodide-xbuildenv-0.30.5/0.28.1/xbuildenv/pyodide-root/dist/$file /pyodide/dist; done

FROM scratch AS export-stage

COPY --from=build-stage /pyodide/dist /
