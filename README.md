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
0	$.names[0].name	"Harry"
0	$.names[1].name	"Hermione"
0	$.names[2].name	"Ron"
0	$.names[3].name	"Neville"
0	$.names[4].name	"Luna"
```
