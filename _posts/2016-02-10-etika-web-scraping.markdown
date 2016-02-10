---
layout: post
title: "Etika Web Scraping"
date: 2016-02-10 20:50:00
categories: concurrency c++ web-scraping
---
Baru-baru ini saya ada praktis concurrency dengan membuat web scraping menggunakan C++11. Sebelum ini saya takut untuk programming menggunakan C++ sebab kena membuat memory management secara manual. Selepas membaca segala best practice untuk C++, barulah nampak yang C++11/C++14 boleh tahan elegant-nya. Yang nampak kurang sedap bila ada orang mengaku pandai C++ tapi programming macam C.

Berbalik ke cerita web scraping, C++ memang elegant, sehinggalah kena guna library C. Kalau tak jumpa wrapper C++ untuk library tersebut, mulut memang akan sumpah seranah waktu programming. Saya menggunakan library 'libcurl' untuk mendapatkan HTML daripada website, 'libxml2' untuk parse code HTML, dan library Boost untuk membuat concurrency.

'libcurl' dan 'libxml2' memang sudah ada wrapper C++, tetapi untuk educational purpose, saya cuba untuk wrap sendiri library tersebut menggunakan kaedah Resource Acquisition Is Initialization (RAII). Kaedah RAII memang selalu digunakan dalam C++ untuk memudahkan memory management.

Semasa cubaan awal, segalanya berjalan lancar. Boleh dapatkan code HTML daripada website, dan boleh parse HTML untuk dapatkan data yang diingini. Cabaran bermula apabila saya mula membuat concurrency. Apabila run sahaja, terus dapat segfault (segmentation fault). Kalau yang dah biasa, memang akan tahu yang segfault mesti punca daripada masalah memory.

Jadi, macam-macamlah yang saya ubahsuai code untuk menyelesaikan masalah. Tukar daripada kaedah menggunakan thread seperti biasa, kepada menggunakan future/promise, dan akhir sekali kepada menggunakan barrier. Selepas meletakkan barrier, saya perasan ada data yang saya dapat fetch sampai satu tahap barulah akan segfault seperti biasa.

Kemudian saya pun tengoklah macam mana rupa code HTML yang saya fetch tersebut. Barulah perasan yang saya dah kena block daripada website tersebut. Seperti semua sedia maklum, program C++ sememangnya laju. Jadi web scraping dengan concurrency akan jadi amat laju dan berpotensi membuatkan pemilik website tersebut berasa bimbang, lantas akan menghalang kita daripada mengakses website mereka.

Kesimpulannya, moral of the story untuk kisah ini ialah kita mestilah beretika semasa membuat web scraping. Seboleh-bolehnya jangan memaksa sangat website tersebut untuk handle request daripada program kita. Tak perlu laju-laju sangat pun takpe, janji dapat data yang dihajati. Kalau nak lagi elok, minta izin daripada pemilik website tersebut untuk membuat web scraping, lagi bagus.
