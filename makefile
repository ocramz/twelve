LTS_VERSION=16.25
STACK_RESOLVER=lts-${LTS_VERSION}

# tag of the docker development image
IMAGE_DEV=ocramz/hs-dev-${STACK_RESOLVER}

DOCKERDEPLOY_DIR=artifacts
## local directory for project deployment binaries
ARTIFACTS_DEPLOY_DIR=${PWD}/${DOCKERDEPLOY_DIR}

setup:
	stack exec -- twelve init
	cp assets/* _templates/

clean:
	rm -rf _templates _site twelve.json

# 1) build the development image
docker-dev-build:
	docker build -t ${IMAGE_DEV} docker/dev

dkr-install:
	stack install --docker --no-interleaved-output --resolver ${STACK_RESOLVER} --local-bin-path ${ARTIFACTS_DEPLOY_DIR}
