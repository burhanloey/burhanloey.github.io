---
layout: post
title: "Misi Mencari IDE Untuk Go"
date: 2015-10-22 16:42:00
categories: go
---
Saya memilih Go sebab saya perlukan programming language yang strict. Walaupun Go masih baru, Go menepati citarasa saya, antaranya static-typing. Kelebihan static-typing language adalah supaya dapat manfaat daripada IDE, seperti code auto-completion. Disebabkan Go masih baru, support daripada IDE tidaklah setanding dengan Java. Cari punya cari, akhirnya saya jumpa text editor yang boleh menjadi IDE untuk Go, iaitu [Atom](https://atom.io/).

Untuk menggunakan Atom untuk Go, saya menambah package [go-plus](https://atom.io/packages/go-plus). Selepas selesai install package tersebut, saya download functionality yang diperlukan seperti `gocode`, `gofmt`, `goimports`, `go vet`, `golint`, `go build` dan `go test`.

Sebelum ini saya menggunakan [LiteIDE](https://github.com/visualfc/liteide). LiteIDE juga bagus, tetapi disebabkan saya menggunakan cloud hosting Google App Engine, ada beberapa package yang IDE tersebut 'tak nampak'. Jadi, package `appengine` dan yang berkaitan tidak dapat support daripada code auto-completion.

Untuk membuatkan Atom boleh nampak package `appengine`, saya perlu set library path untuk `gocode` dengan cara:
```
gocode set lib-path c:\go_appengine\goroot\pkg\windows_amd64_appengine
```

Sekarang barulah ada code auto-completion untuk package `appengine`.

Satu lagi masalah ialah apabila save file program Go, akan keluar error yang menyatakan `src/appengine` tidak berada dalam `$GOPATH` dan `$GOROOT`. Untuk menyelesaikan masalah ini, saya membuat symlink:
```
mklink /J $GOPATH/src/appengine c:\go_appengine\goroot\src\appengine
mklink /J $GOPATH/src/appengine_internal c:\go_appengine\goroot\src\appengine_internal
```

[mklink](https://technet.microsoft.com/en-us/library/cc753194.aspx) ialah command dalam Windows untuk membuat symlink. Kalau anda menggunakan operating system yang lain akan berbeza.

Walaupun semua steps di atas agak leceh, saya tetap berpuas hati.

Alternatif kepada semua cara di atas adalah dengan menggunakan package `appengine` yang baru iaitu `google.golang.org/appengine`. Tetapi dengan menggunakan package tersebut, saya tidak dapat code auto-completion jika menggunakan package yang berada di dalamnya seperti `google.golang.org/appengine/user` dan `google.golang.org/appengine/log`.
