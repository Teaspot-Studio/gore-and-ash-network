FROM phadej/ghc:7.10.2 

ADD mkdocs /opt/project/
ADD LICENSE /opt/project/
ADD LICENSE_henet /opt/project/
ADD README_henet.md /opt/project/
ADD Setup.hs /opt/project/
ADD enet /opt/project/enet
ADD henet /opt/project/henet
ADD README.md /opt/project/
ADD gore-and-ash-network.cabal /opt/project/
ADD stack.yaml /opt/project/
ADD src /opt/project/src

WORKDIR /opt/project

ENTRYPOINT ["./mkdocs", "gore-and-ash-network", "1.2.0.0", "NCrashed"]