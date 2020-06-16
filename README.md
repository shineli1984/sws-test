# sws-test

### Install
- Install stack `https://docs.haskellstack.org/en/stable/README/`
- clone the repo

### Run
Backend (stack is setup with docker enabled):
```shell
# run with default options
stack --docker-run-args='--net=bridge --publish=9998:9998' run
```
Backend with customised options:
```shell
stack install
$HOME/.local/bin/sws-test-exe --help
```
this will produce the following:
```
Companies page backend

Usage: sws-test-exe [-c|--connStr CONNSTR] [-s|--connPoolSize INT] 
                    [-o|--allowOrigin ARG] [-p|--port INT]
  Sws test

Available options:
  -c,--connStr CONNSTR     Database connection
                           string (default: "db/sws.sqlite3")
  -s,--connPoolSize INT    Connection pool size (default: 10)
  -o,--allowOrigin ARG     Access-Control-Allow-Origin
                           header (default: "http://localhost:3000")
  -p,--port INT            Port for server (default: 9998)
  -h,--help                Show this help text
```

Frontend:
```shell
cd frontend
npm run
```

### Unit/Property tests
Frontend
```shell
npm test
```