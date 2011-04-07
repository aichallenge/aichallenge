var Fraction = function (num, den) {
    return {
        num: num,
        den: den
    }
};

Fraction.toNumber = function (fraction) {
    return fraction.num / fraction.den;
}

Fraction.add = function (fraction1, fraction2) {
    var lcm = this.lcm(fraction1.den, fraction2.den);
    return {
        num: (fraction1.num * fraction2.den / lcm +
              fraction1.den * fraction2.num / lcm),
        dem: lcm
    };
};

Fraction.gcd = function (a, b) {
    if (a < b) {
        var t = a;
        a = b;
        b = t;
    }
    while (true) {
        r = a % b;
        if (r === 0) {
            return b;
        }
        a = b;
        b = r;
    }
};

Fraction.lcm = function (a, b) {
    return a * b / gcd(a, b);
};