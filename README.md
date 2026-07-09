# Konverter Satuan & Mata Uang dengan Haskell

Program Konverter Satuan & Mata Uang berbasis CLI (Command Line Interface) ini dibuat menggunakan bahasa pemrograman fungsional **Haskell** murni tanpa library eksternal yang rumit. 

## Fitur Utama

Program ini mendukung konversi untuk kategori berikut:
1. **Panjang**: Meter, Kilometer, Centimeter, Millimeter, Inci (Inch), Kaki (Foot), Yard, Mil (Mile).
2. **Berat**: Kilogram, Gram, Miligram, Ton, Pound, Ons (Ounce).
3. **Suhu**: Celsius, Fahrenheit, Kelvin.
4. **Waktu**: Detik, Menit, Jam, Hari.
5. **Mata Uang (15 Mata Uang dengan Kurs Tetap/Hardcoded)**:
   - IDR (Rupiah Indonesia)
   - USD (US Dollar)
   - EUR (Euro)
   - GBP (Pound Sterling)
   - JPY (Yen Jepang)
   - CNY (Yuan China)
   - KRW (Won Korea)
   - SGD (Dollar Singapura)
   - MYR (Ringgit Malaysia)
   - THB (Baht Thailand)
   - AUD (Dollar Australia)
   - CAD (Dollar Kanada)
   - CHF (Franc Swiss)
   - INR (Rupee India)
   - SAR (Riyal Arab Saudi)

## Konsep Haskell yang Digunakan

- **Function & Type Signatures**: Setiap fungsi memiliki tipe data yang eksplisit.
- **Pattern Matching & Guards**: Digunakan untuk menentukan logika konversi dan alur menu secara terstruktur.
- **Rekursi**: Digunakan untuk perulangan menu utama (`mainMenu`) sehingga program terus berjalan hingga pengguna memilih menu Keluar (`0`).
- **Modularisasi**: Memisahkan logika murni (`Converter.hs`) dari penanganan input-output / UI (`Main.hs`).
- **Validasi Input**:
  - Validasi pilihan menu tidak tersedia.
  - Validasi input kosong.
  - Validasi input nilai negatif untuk satuan fisik (Panjang, Berat, Waktu, Mata Uang).
  - Validasi suhu Kelvin tidak boleh di bawah 0 mutlak (0 K).

## Struktur Project

```
Konverter-Haskell/
│
├── Main.hs          # Program Utama (Tampilan Menu, Input, dan Output)
├── Converter.hs     # Logika Konversi Satuan dan Mata Uang (Pure Functions)
└── README.md        # Dokumentasi Cara Penggunaan Program
```

## Persyaratan Sistem

Pastikan Anda memiliki **GHC (Glasgow Haskell Compiler)** terinstal pada sistem Anda. Jika belum, Anda bisa mendapatkannya melalui [GHCup](https://www.haskell.org/ghcup/).

## Cara Menjalankan Program

Ada dua cara utama untuk menjalankan program ini:

### 1. Menjalankan secara Langsung menggunakan Interpreter
Buka terminal/command prompt pada folder project dan jalankan perintah:
```bash
runghc Main.hs
```

### 2. Menggunakan GHCi (Interactive Shell)
Buka GHCi dan load file `Main.hs`:
```bash
ghci Main.hs
```
Setelah GHCi berhasil memuat modul, ketik perintah berikut untuk memulai program:
```haskell
main
```

### 3. Mengompilasi Menjadi File Executable
Untuk mengompilasi program menjadi file binary mandiri, gunakan perintah:
```bash
ghc Main.hs -o Konverter
```
Setelah berhasil dikompilasi, jalankan program binary yang dihasilkan:
- **Windows**: `Konverter.exe`
- **Linux/macOS**: `./Konverter`

## Contoh Penggunaan

### Contoh 1: Konversi Panjang
```
Pilih menu : 1

=====================================
          KONVERSI PANJANG           
=====================================
1. Meter
2. Kilometer
3. Centimeter
4. Millimeter
5. Inch
6. Foot
7. Yard
8. Mile

Pilih satuan asal (1-8): 1
Pilih satuan tujuan (1-8): 2
Masukkan nilai: 2500

=====================================
            HASIL KONVERSI           
=====================================
2500 Meter
=
2.5 Kilometer
=====================================
```

### Contoh 2: Konversi Mata Uang
```
Pilih menu : 5

=====================================
         KONVERSI MATA UANG          
=====================================
... (daftar 15 mata uang) ...

Pilih mata uang asal (1-15): 2 (USD)
Pilih mata uang tujuan (1-15): 1 (IDR)
Masukkan nilai uang: 10

=====================================
            HASIL KONVERSI           
=====================================
10 USD
=
163000 IDR
=====================================
```
