# sws-test

### Install
- Install stack `https://docs.haskellstack.org/en/stable/README/`
- clone the repo

### Run
Backend (stack is setup with docker enabled):
```Shell
stack --docker-run-args='--net=bridge --publish=9998:9998' ghci
ghci> main
```
Frontend:
```Shell
cd frontend
npm run
```

### Unit/Property tests
Backend:
```Shell
stack test
```
Frontend
```Shell
npm test
```