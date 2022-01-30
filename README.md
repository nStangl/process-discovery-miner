# process-mining

Repository for my bachelor internshipiImplementation of process discovery algorithms. Implementing a full stack process discovery miner inlcuding an Alpha-Miner.

# Overview

The process miner expects a log as a .xml or [.xes](https://www.xes-standard.org/openxes/start) file. The log cannot exceed 50 MB in size (can be changed). The process minig algorithms can also merely be used as APIs, for example:

```
curl -X POST http://localhost:3000/api/v1/alphaminer -H "Content-Type: application/xml" -H "Accept: application/xml" --data @L1.xes > result.json
```
Currently supported are /api/v1/{alphaminer | alphaplusminer}

(L1.xes refers to a file in the current directory. Make sure to change the URL.)

## Examples

Here are some examples of how the logs are supposed to be structured:
<p>
<details>
<summary>Minimal log</summary>

A log can have multiple traces and each trace can have multiple events (but each at least one).

```
 <?xml version="1.0" encoding="UTF-8" ?>
<log xes.version="1.0" xes.features="nested-attributes" openxes.version="1.0RC7" xmlns="http://www.xes-standard.org/">
    <trace>
        <string key="concept:name" value="Case0"/>
        <event>
            <string key="concept:name" value="a"/> 
        </event>
        <event>
            <string key="concept:name" value="b"/>
        </event>
        <event>
			<string key="concept:name" value="c"/>
        </event>
    </trace>
</log>
```

</details>
</p>

### Using Lifecycle Attributes

The in the Open XES standard defined lifecycle attributes can be used as follows:
- If the events have defined lifecycle attributes, only the events with the attribute "complete" will be used. All other lifecycle attributes will be skipped.
- The XES extension, classifier and global tags are ignored when parsing the log. They don't have to be included.

# Setup

The project uses the Haskell webframework Yesod with the Warp backend to function as a webserver. The frontend is using React.js with Material UI and Typescript.

## Frontend

1. Installing Typescript: `npx tsx`
2. Install dependecies: `cd static/app && npm install`
3. Build via Webpack: `npm run build`

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.



### Development

Start a development server with:

```
stack exec -- yesod devel
```

If your are making changes to the frontend, make sure you rebuild it via webpack. If you are running the devel development server it should notice the change and rebuild. If not or you want to have a clean build make sure to:
```
stack clean && stack build
```
Or:
```
stack clean && stack exec -- yesod devel
```

### Tests

If you want to test the process discovery miner make sure to execute the `install` script (located in /test/logs/) to download a collection of logs (and `delete` to remove afterwards). Then execute:

```
stack test
```
Interesting side note: stack test can also measure code coverage, i.e.
```
stack test --coverage
```
and then look into your build directory for the index.html.

### Executing

If you want to run the server, execute:
```
stack exec process-miner
```
The host, port and other settings are located in /config/settings.yml.
If you want to run it as a detached process (in the background):
```
stack exec process-miner > logfile.txt &
```
If you merely want to test quick changes, you can also do:
```
stack exec -- yesod devel --port 8002 --tls-port 8003 --host '::'
```
See `stack exec -- yesod devel --help` for more information.