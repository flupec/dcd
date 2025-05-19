## Interview process simplifier

This CLI tool helps with employee candidate competencies estimation.

## Usage

Firstly, you need to write dcd file. Dcd file contains information about desired employee competencies.
Competency is a tree-like structure with sub competencies, questions and answers attached to tree node.

### Start TUI session to estimate candidate competency

```
dcd estimate -f <competency-src-file> -l <candidate-lastname>
```

During TUI session you can estimate employee knowledge in percents. Tool automatically calculates parent
estimates in a tree - but you can override those estimations.

After interview end you can export results of competencies estimation. Those results
will be available at `$HOME` or by path specified in `DCD_EXPORT_PATH` variable.

### Generate competencies report

```
dcd report -s <src-descriptor-file> -t <dest-file> -r <result-file> -r <result-file>...
```

This command will generate competencies report in PDF format.
