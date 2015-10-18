---
layout: post
title: "Pertama Kali Menggunakan 'Closure'"
date: 2015-10-18 10:55:00
categories: javascript
---
Saya masih baru dalam web development dan functional programming. Semasa belajar functional programming dengan Scala, ada beberapa konsep dan teknik yang rasa-rasa macam faham, rasa-rasa macam tak, rasa-rasa tak pasti, antaranya lambda expression, map reduce, closure, dan monad.

![I don't get it]({{ site.url }}/assets/i-dont-get-it.png)

Semuanya terjawab sedikit demi sedikit apabila saya mula menggunakan JavaScript. Dalam tutorial Scala, kita terus menggunakan simbol-simbol seperti simbol lambda, jadi kita tak beberapa nak nampak apa yang berlaku under the hood semasa menggunakan simbol lambda. Berlainan dengan JavaScript, di mana kita perlu menulis anonymous function untuk membuat map reduce tanpa menggunakan simbol lambda. Lambda expression untuk JavaScript akan ada support dalam ECMAScript 6 nanti.

Berbalik pada tajuk asal, antara feature dalam functional programming ialah closure. Kalau tengok pada mana-mana tutorial, contoh yang mereka beri adalah untuk menunjukkan bagaimana return function boleh akses variable dalam function yang lain. Jadi, saya pun tertanya-tanya, "Apa yang special-nya?", saya boleh sahaja letak variable tersebut sebagai global dan akses dari mana-mana sahaja, betul tak? Pendapat saya itu kekal sehinggalah saya cuba membuat web development, barulah saya nampak betapa awesome-nya konsep closure ini.

Sebelum itu, saya nak tunjuk masalah apa yang saya nak selesaikan, iaitu sign up form. Jangan gelak, saya masih baru.

Sign up form yang saya nak buat simple sahaja. Ada tiga textfield untuk username, password, dan verify password. Untuk kedua-dua textfield password, saya nak display icon untuk menunjukkan sama ada input tersebut dibenarkan ataupun tidak sebaik sahaja user tulis sesuatu. Untuk textfield password, input yang saya benarkan hanyalah 6 character minimum. Untuk textfield verify password pula, input yang saya benarkan ialah input yang sama dengan password(obviously).

Saya menggunakan jQuery, jadi pada mulanya saya tulis code seperti ini:

{% highlight javascript %}
// Check password length. Toggle icon accordingly. Password must be at least 
// 6 characters. 
function validatePassword() {
    if ($("#password-input").val().length >= 6) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}

// Check if password verification is the same with password. Toggle icon
// accordingly.
function verifyPassword() {
    if ($("#verify-password-input").val() === $("#password-input").val()) {
        $("#verify-password-check").removeClass("glyphicon-remove");
        $("#verify-password-check").addClass("glyphicon-ok");
    } else {
        $("#verify-password-check").removeClass("glyphicon-ok");
        $("#verify-password-check").addClass("glyphicon-remove");
    }
}

// Show icon if the input is not null, and then validate.
function showIconAndValidate() {
    if ($("#password-input").val().length > 0) {
        $("#password-check").show(0, validatePassword);
    } else {
        $("#password-check").hide(0);
    }
}

// Show icon if the input is not null, and then verify.
function showIconAndVerify() {
    if ($("#verify-password-input").val().length > 0) {
        $("#verify-password-check").show(0, verifyPassword);
    } else {
        $("#verify-password-check").hide(0);
    }
}

// Set-up events.
$("#password-input").keyup(showIconAndValidate);
$("#verify-password-input").keyup(showIconAndVerify);
{% endhighlight %}

Kalau sesiapa yang dah pro mungkin boleh nampak, ada bahagian yang tidak menggunakan konsep DRY(Don't-Repeat-Yourself). Sebelum membuat validation dan verification, saya check sama ada textfield ada input ataupun tidak. Kalau ada display, kalau tiada hide. Bahagian tersebut saya tulis dua kali, satu untuk textfield password, satu lagi untuk textfield verify password, jadi tak berapa bagus di situ.

Oleh itu, saya perlu membuat satu function untuk handle sama ada textfield ada input ataupun tidak. Tapi, tunggu dulu. Perhatikan selepas checking tersebut, saya ada membuat callback. Callback validatePassword() untuk password, callback verifyPassword() untuk verify password. Jadi, bagaimana kita nak asingkan callback tersebut? Ya, di sinilah saya menggunakan closure.

Code yang baru selepas refactor akan jadi seperti ini:

{% highlight javascript %}
// Check password length. Toggle icon accordingly. Password must be at least 
// 6 characters. 
function validatePassword() {
    if ($("#password-input").val().length >= 6) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}

// Check if password verification is the same with password. Toggle icon
// accordingly.
function verifyPassword() {
    if ($("#verify-password-input").val() === $("#password-input").val()) {
        $("#verify-password-check").removeClass("glyphicon-remove");
        $("#verify-password-check").addClass("glyphicon-ok");
    } else {
        $("#verify-password-check").removeClass("glyphicon-ok");
        $("#verify-password-check").addClass("glyphicon-remove");
    }
}

// Show icon if the input is not null.
function showIcon(callback) {
    return function() {
        var input = $(this);
        var icon = $(this).parent().next();
        
        if (input.val().length > 0) {
            icon.show(0, callback);
        } else {
            icon.hide(0);
        }
    };
}

// Set-up events.
$("#password-input").keyup(showIcon(validatePassword));
$("#verify-password-input").keyup(showIcon(verifyPassword));
{% endhighlight %}

Saya menggunakan JavaScript traversing, iaitu `$(this).parent().next()` untuk pilih icon. Ini tidak ada kena mengena dengan closure. Baca seterusnya untuk penjelasan mengenai closure.

Dalam jQuery, kita boleh passing function sebagai callback. Dalam JavaScript, jika kita letak kurungan selepas function, secara automatik kita akan invoke function tersebut. Dalam masalah saya ini, saya letak function sebagai parameter untuk callback. Jadi kalau saya tidak membuat closure, apabila saya pass function sebagai argument untuk function showIcon(), secara automatik saya invoke showIcon(), tetapi saya tak mahu begitu. Saya mahu jQuery yang invoke showIcon() bergantung kepada event yang saya tetapkan. Jadi, dengan menggunakan closure, saya boleh pass function showIcon() yang dah 'combined' dengan function yang lain.

Awesome kan?

P/S: Perhatikan function validatePassword() dan verifyPassword() ada bahagian yang berulang. Saya serahkan kepada pembaca untuk selesaikan masalah tersebut. :)
