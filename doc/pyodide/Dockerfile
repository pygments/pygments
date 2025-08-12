# Dockerfile for building Pyodide with a Pygments version from the current checkout.
# This is used by the `pyodide` tox environment (see /tox.ini).
FROM ubuntu:25.04 AS build-stage

RUN apt update && apt install -y python3 python3-pip
# Pin the version to avoid surprises...
RUN pip install --break-system-packages pyodide-build==0.30.5
RUN pyodide xbuildenv install 0.28.1

RUN apt install -y git

RUN git clone https://github.com/emscripten-core/emsdk /emsdk
RUN cd emsdk && PYODIDE_EMSCRIPTEN_VERSION=$(pyodide config get emscripten_version) && ./emsdk install ${PYODIDE_EMSCRIPTEN_VERSION} && ./emsdk activate ${PYODIDE_EMSCRIPTEN_VERSION}

# Copy new meta with path to local Pygments instead of pypi url.
COPY . /pygments
RUN sed -i -e 's,dynamic =.*,version = "2.99",' /pygments/pyproject.toml

WORKDIR /pyodide
COPY doc/pyodide/meta.yaml packages/Pygments/
RUN cd /emsdk && . ./emsdk_env.sh && cd /pyodide && pyodide build-recipes Pygments --install
RUN for file in pyodide.js pyodide.asm.js pyodide.asm.wasm python_stdlib.zip; do cp /root/.cache/.pyodide-xbuildenv-0.30.5/0.28.1/xbuildenv/pyodide-root/dist/$file /pyodide/dist; done

FROM scratch AS export-stage

COPY --from=build-stage /pyodide/dist /
