---
layout: post
title: "Pertama Kali Menggunakan 'Closure'(sambungan...)"
date: 2015-10-19 16:42:00
categories: javascript
---
Sebelum ini saya ada tunjuk bagaimana kita boleh menggunakan closure untuk menggabungkan satu function dengan function yang lain. Di bahagian paling bawah post tersebut, saya ada meminta pembaca untuk selesaikan code yang masih ada bahagian berulang. Jika anda belum mencuba dan ingin mencuba boleh lihat di [post ini]({{ site.url }}//javascript/2015/10/18/pertama-kali-menggunakan-closure.html). Jika tak mahu, boleh teruskan membaca untuk lihat cara saya menyelesaikannya.

Jadi, kenapa kena guna closure? Saje, supaya code nampak lebih elegant. Kalau boleh semua function saya nak guna closure.

![Write all the functions with closure!!!]({{ site.url }}/assets/all-closures.png)

Baiklah. Code sebelum ini masih ada bahagian yang berulang, iaitu:

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
{% endhighlight %}

Tetapi masalahnya condition untuk if else antara kedua-dua function tersebut adalah berlainan.

Dalam functional programming, semua benda kita boleh jadikan function, dan kita juga boleh passing function melalui parameter. Jadi, secara teorinya kita akan jadikan condition untuk if else tersebut sebagai function, kemudian kita akan hantar function tersebut melalui parameter untuk closure. Saya akan tunjukkan perlahan-lahan menggunakan salah satu function tersebut.

Function yang asal adalah seperti ini:

{% highlight javascript %}
function validatePassword() {
    if ($("#password-input").val().length >= 6) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}
{% endhighlight %}

Sekarang kita asingkan condition tersebut ke dalam satu variable:

{% highlight javascript %}
function validatePassword() {
    var ok = $("#password-input").val().length >= 6;

    if (ok) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}
{% endhighlight %}

Dan untuk menjadikannya sebagai function, kita boleh menggunakan singleton yang akan self-invoke:

{% highlight javascript %}
function validatePassword() {
    var ok = (function() { return $("#password-input").val().length >= 6; })();

    if (ok) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}
{% endhighlight %}

Akhirnya, kita sudah menjadikan condition tersebut sebagai sebuah function, dan kita boleh asingkan condition tersebut di luar, seperti berikut:

{% highlight javascript %}
function() {
    return $("#password-input").val().length >= 6;
}

function validatePassword(condition) {
    var ok = condition();

    if (ok) {
        $("#password-check").removeClass("glyphicon-remove");
        $("#password-check").addClass("glyphicon-ok");
    } else {
        $("#password-check").removeClass("glyphicon-ok");
        $("#password-check").addClass("glyphicon-remove");
    }
}
{% endhighlight %}

Masalah dengan cara ini ialah apabila kita hantar condition melalui parameter, kita akan invoke function validatePassword() secara automatik. Untuk tidak invoke function tersebut, kita boleh menggunakan closure:

{% highlight javascript %}
function() {
    return $("#password-input").val().length >= 6;
}

function validatePassword(condition) {
    return function() {
        var ok = condition();
        
        if (ok) {
            $("#password-check").removeClass("glyphicon-remove");
            $("#password-check").addClass("glyphicon-ok");
        } else {
            $("#password-check").removeClass("glyphicon-ok");
            $("#password-check").addClass("glyphicon-remove");
        }
    };
}
{% endhighlight %}

Sekarang untuk menyelesaikan masalah code yang berulang, kita boleh abstract-kan function menggunakan keyword `this`, seperti berikut:

{% highlight javascript %}
function toggleIcon(condition) {
    return function() {
        var ok = condition();
        var icon = $(this);
        
        if (ok) {
            icon.removeClass("glyphicon-remove");
            icon.addClass("glyphicon-ok");
        } else {
            icon.removeClass("glyphicon-ok");
            icon.addClass("glyphicon-remove");
        }
    };
}
{% endhighlight %}

Kalau mahu lebih ringkas, boleh tulis seperti ini:

{% highlight javascript %}
function toggleIcon(ok) {
    return function() {
        var icon = $(this);
        
        if (ok()) {
            icon.removeClass("glyphicon-remove");
            icon.addClass("glyphicon-ok");
        } else {
            icon.removeClass("glyphicon-ok");
            icon.addClass("glyphicon-remove");
        }
    };
}
{% endhighlight %}

Note: Icon boleh di-refer menggunakan `$(this)`, dan bukannya seperti sebelum ini di mana kita menggunakan `$(this).parent().next()` kerana function tersebut adalah callback kepada event untuk icon itu sendiri. Kalau nak tukar scope, boleh guna `bind()`, tapi itu dah lari dari topik.

Sekarang kita sudah ada closure untuk handle toggle icon.

Selepas refactor, akhir sekali code kita akan jadi seperti ini:

{% highlight javascript %}
// Condition that states that password input must be minimum of 6 characters.
function validate() {
    return $("#password-input").val().length >= 6;
}

// Condition that states that password input must be the same as verify password
// input.
function verify() {
    return $("#verify-password-input").val() === $("#password-input").val();
}

// Toggle icon according to condition. If true, shows ok. If false, shows cross.
function toggleIcon(ok) {
    return function() {
        var icon = $(this);
        
        if (ok()) {
            icon.removeClass("glyphicon-remove");
            icon.addClass("glyphicon-ok");
        } else {
            icon.removeClass("glyphicon-ok");
            icon.addClass("glyphicon-remove");
        }
    };
}

// Show icon if the input is not null.
function showIcon(duration, callback) {
    return function() {
        var input = $(this);
        var icon = $(this).parent().next();
        
        if (input.val().length > 0) {
            icon.show(duration, callback);
        } else {
            icon.hide(duration);
        }
    };
}

// Set-up events.
$("#password-input").keyup(showIcon(0, toggleIcon(validate)));
$("#verify-password-input").keyup(showIcon(0, toggleIcon(verify)));
{% endhighlight %}

Bandingkan dengan code sebelum ini, yang mana lebih anda suka?

Kesimpulannya, closure is awesome. Seperti yang tertulis di Mozilla, kita boleh menggunakan closure sebagai 'function factory'. Anda boleh baca dengan lebih lanjut di contoh makeAdder di [sini](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Closures#Closure).
