// Advent of Code 2016, day 7: Internet Protocol Version 7
// https://adventofcode.com/2016/day/7

use itertools::Itertools;

fn is_palindrome(s: &[u8]) -> bool {
    std::iter::zip(s.iter(), s.iter().rev()).all(|(x, y)| x == y) && !s.iter().take(2).all_equal()
}

fn has_abba(input: &str) -> bool {
    input.as_bytes().windows(4).any(is_palindrome)
}

fn filter_palindromes(input: &str) -> Vec<&[u8]> {
    input
        .as_bytes()
        .windows(3)
        .filter(|x| is_palindrome(x))
        .collect()
}

fn supports_tls(input: &str) -> bool {
    let mut supernet_seq = false;
    let mut abba_ok = false;

    for s in input.split(|c| c == ']' || c == '[') {
        supernet_seq = !supernet_seq;

        if has_abba(s) {
            if supernet_seq {
                abba_ok = true;
            } else {
                abba_ok = false;
                break;
            }
        }
    }
    abba_ok
}

fn supports_ssl(input: &str) -> bool {
    let mut supernet_seq = false;
    let mut abas = Vec::new();
    let mut babs = Vec::new();

    for s in input.split(|c| c == ']' || c == '[') {
        supernet_seq = !supernet_seq;
        let mut ps = filter_palindromes(s);
        if supernet_seq {
            abas.append(&mut ps);
        } else {
            babs.append(&mut ps);
        }
    }

    for a in abas {
        for b in &babs {
            if a[0] == b[1] && b[0] == a[1] {
                return true;
            }
        }
    }

    false
}

pub fn run(input: &str) {
    println!("{}", input.lines().filter(|l| supports_tls(l)).count());
    println!("{}", input.lines().filter(|l| supports_ssl(l)).count());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn palindrome_test() {
        assert_eq!(is_palindrome("abba".as_bytes()), true);
        assert_eq!(is_palindrome("aba".as_bytes()), true);
        assert_eq!(is_palindrome("abbd".as_bytes()), false);
        assert_eq!(is_palindrome("aaaa".as_bytes()), false);
    }

    #[test]
    fn first_3palindrome_test() {
        assert_eq!(filter_palindromes("zaba"), ["aba".as_bytes()]);
        assert_eq!(
            filter_palindromes("zabab"),
            ["aba".as_bytes(), "bab".as_bytes()]
        );
    }

    #[test]
    fn abba_test() {
        assert_eq!(has_abba("zabbaddd"), true);
        assert_eq!(has_abba("zabbddd"), false);
    }

    #[test]
    fn tls_test() {
        assert_eq!(supports_tls("abba[mnop]qrst"), true);
        assert_eq!(supports_tls("abcd[bddb]xyyx"), false);
        assert_eq!(supports_tls("aaaa[qwer]tyui"), false);
        assert_eq!(supports_tls("ioxxoj[asdfgh]zxcvbn"), true);
    }

    #[test]
    fn ssl_test() {
        assert_eq!(supports_ssl("aba[bab]xyz"), true);
        assert_eq!(supports_ssl("xyx[xyx]xyx"), false);
        assert_eq!(supports_ssl("aaa[kek]eke"), true);
        assert_eq!(supports_ssl("zazbz[bzb]cdb"), true);
    }
}
