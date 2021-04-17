# flatter

A CLI tool to flatten YAML structures (documents and multi-documents).

```
$ flatter
names:
- name: Harry
- name: Hermione
- name: Ron
- name: Neville
- name: Luna
^D
1	$.names[0].name	"Harry"
1	$.names[1].name	"Hermione"
1	$.names[2].name	"Ron"
1	$.names[3].name	"Neville"
1	$.names[4].name	"Luna"
```

Flattening the file structure to one key/value-entry per line makes it
easier to perform some kinds of queries with standard Unix-like CLI
tooling, e.g.:

```
$ flatter --format=json < factbook.json | grep government.capital.name | cut -f 3 | sort | head -5
"Abu Dhabi"
"Abuja"
"Accra"
"Adamstown"
"Addis Ababa"
```

```
$ kubectl get pod -o yaml | flatter | grep .hostIP | cut -f 3 | sort | uniq -c | sort -n
      9 "10.128.0.58"
     11 "10.128.0.42"
     11 "10.128.0.59"
```

Using the `--reverse` argument, this approach can also be used for
transformations:

```
$ flatter | grep -v tybalt | sed "s/family/clan/" | flatter --reverse
personae:
  romeo:
    family: Montague
  juliet:
    family: Capulet
  tybalt:
    family: Capulet
^D
---
personae:
  romeo:
    clan: Montague
  juliet:
    clan: Capulet
```
