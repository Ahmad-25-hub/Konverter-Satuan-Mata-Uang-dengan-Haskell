module Converter (
    SatuanPanjang(..),
    SatuanBerat(..),
    SatuanSuhu(..),
    SatuanWaktu(..),
    MataUang(..),
    daftarPanjang,
    daftarBerat,
    daftarSuhu,
    daftarWaktu,
    daftarMataUang,
    convertLength,
    convertWeight,
    convertTemperature,
    convertTime,
    convertCurrency
) where

-- ============================================================================
-- 1. DEFINISI TIPE DATA (ADTs) & LIST DAFTAR SATUAN
-- ============================================================================

-- Tipe data untuk satuan Panjang
data SatuanPanjang = Meter | Kilometer | Centimeter | Millimeter | Inch | Foot | Yard | Mile
    deriving (Show, Eq, Enum, Bounded)

-- Tipe data untuk satuan Berat
data SatuanBerat = Kilogram | Gram | Milligram | Ton | Pound | Ounce
    deriving (Show, Eq, Enum, Bounded)

-- Tipe data untuk satuan Suhu
data SatuanSuhu = Celsius | Fahrenheit | Kelvin
    deriving (Show, Eq, Enum, Bounded)

-- Tipe data untuk satuan Waktu
data SatuanWaktu = Detik | Menit | Jam | Hari
    deriving (Show, Eq, Enum, Bounded)

-- Tipe data untuk 15 mata uang yang didukung
data MataUang = IDR -- Rupiah Indonesia
              | USD -- Dollar Amerika Serikat
              | EUR -- Euro
              | GBP -- Pound Sterling
              | JPY -- Yen Jepang
              | CNY -- Yuan China
              | KRW -- Won Korea Selatan
              | SGD -- Dollar Singapura
              | MYR -- Ringgit Malaysia
              | THB -- Baht Thailand
              | AUD -- Dollar Australia
              | CAD -- Dollar Kanada
              | CHF -- Franc Swiss
              | INR -- Rupee India
              | SAR -- Riyal Arab Saudi
    deriving (Show, Eq, Enum, Bounded)

-- Menggunakan Enum & Bounded untuk mendapatkan daftar opsi secara otomatis
daftarPanjang :: [SatuanPanjang]
daftarPanjang = [minBound .. maxBound]

daftarBerat :: [SatuanBerat]
daftarBerat = [minBound .. maxBound]

daftarSuhu :: [SatuanSuhu]
daftarSuhu = [minBound .. maxBound]

daftarWaktu :: [SatuanWaktu]
daftarWaktu = [minBound .. maxBound]

daftarMataUang :: [MataUang]
daftarMataUang = [minBound .. maxBound]


-- ============================================================================
-- 2. FUNGSI LOGIKA KONVERSI SATUAN
-- ============================================================================

-- A. Konversi Panjang (Menggunakan basis Meter)
-- Mengembalikan faktor perkalian dari satuan tertentu ke Meter.
faktorPanjangKeMeter :: SatuanPanjang -> Double
faktorPanjangKeMeter Meter      = 1.0
faktorPanjangKeMeter Kilometer  = 1000.0
faktorPanjangKeMeter Centimeter = 0.01
faktorPanjangKeMeter Millimeter = 0.001
faktorPanjangKeMeter Inch       = 0.0254      -- 1 inch = 0.0254 meter
faktorPanjangKeMeter Foot       = 0.3048      -- 1 foot = 0.3048 meter
faktorPanjangKeMeter Yard       = 0.9144      -- 1 yard = 0.9144 meter
faktorPanjangKeMeter Mile       = 1609.344    -- 1 mile = 1609.344 meter

-- Konversi panjang: ubah nilai dari satuan 'asal' ke 'tujuan'
convertLength :: SatuanPanjang -> SatuanPanjang -> Double -> Double
convertLength asal tujuan nilai =
    let nilaiDalamMeter = nilai * faktorPanjangKeMeter asal
    in nilaiDalamMeter / faktorPanjangKeMeter tujuan


-- B. Konversi Berat (Menggunakan basis Gram)
-- Mengembalikan faktor perkalian dari satuan tertentu ke Gram.
faktorBeratKeGram :: SatuanBerat -> Double
faktorBeratKeGram Kilogram  = 1000.0
faktorBeratKeGram Gram      = 1.0
faktorBeratKeGram Milligram = 0.001
faktorBeratKeGram Ton       = 1000000.0
faktorBeratKeGram Pound     = 453.59237      -- 1 pound = 453.59237 gram
faktorBeratKeGram Ounce     = 28.349523125   -- 1 ounce = 28.349523125 gram

-- Konversi berat: ubah nilai dari satuan 'asal' ke 'tujuan'
convertWeight :: SatuanBerat -> SatuanBerat -> Double -> Double
convertWeight asal tujuan nilai =
    let nilaiDalamGram = nilai * faktorBeratKeGram asal
    in nilaiDalamGram / faktorBeratKeGram tujuan


-- C. Konversi Suhu (Menggunakan Celsius sebagai perantara)
-- Mengonversi satuan asal ke Celsius
suhuKeCelsius :: SatuanSuhu -> Double -> Double
suhuKeCelsius Celsius nilai    = nilai
suhuKeCelsius Fahrenheit nilai = (nilai - 32) * 5 / 9
suhuKeCelsius Kelvin nilai     = nilai - 273.15

-- Mengonversi dari Celsius ke satuan tujuan
celsiusKeSuhu :: SatuanSuhu -> Double -> Double
celsiusKeSuhu Celsius nilai    = nilai
celsiusKeSuhu Fahrenheit nilai = (nilai * 9 / 5) + 32
celsiusKeSuhu Kelvin nilai     = nilai + 273.15

-- Konversi suhu: ubah nilai dari satuan 'asal' ke 'tujuan'
convertTemperature :: SatuanSuhu -> SatuanSuhu -> Double -> Double
convertTemperature asal tujuan nilai =
    celsiusKeSuhu tujuan (suhuKeCelsius asal nilai)


-- D. Konversi Waktu (Menggunakan basis Detik)
-- Mengembalikan faktor perkalian dari satuan tertentu ke Detik.
faktorWaktuKeDetik :: SatuanWaktu -> Double
faktorWaktuKeDetik Detik = 1.0
faktorWaktuKeDetik Menit = 60.0
faktorWaktuKeDetik Jam   = 3600.0
faktorWaktuKeDetik Hari  = 86400.0

-- Konversi waktu: ubah nilai dari satuan 'asal' ke 'tujuan'
convertTime :: SatuanWaktu -> SatuanWaktu -> Double -> Double
convertTime asal tujuan nilai =
    let nilaiDalamDetik = nilai * faktorWaktuKeDetik asal
    in nilaiDalamDetik / faktorWaktuKeDetik tujuan


-- ============================================================================
-- 3. FUNGSI LOGIKA KONVERSI MATA UANG
-- ============================================================================

-- Nilai tukar (kurs tetap) 1 unit Mata Uang ke Rupiah (IDR).
-- Data diperbarui: 8-9 Juli 2026 (sumber: BCA e-Rate & Bank Indonesia/Databoks, kurs tengah)
kursKeIDR :: MataUang -> Double
kursKeIDR IDR = 1.0
kursKeIDR USD = 18048.5
kursKeIDR EUR = 19935.0
kursKeIDR GBP = 22858.0
kursKeIDR JPY = 108.38      -- 1 Yen Jepang = Rp108,38
kursKeIDR CNY = 2460.0      -- 1 Yuan China = Rp2.460
kursKeIDR KRW = 11.2        -- 1 Won Korea = Rp11,2
kursKeIDR SGD = 13361.0
kursKeIDR MYR = 4349.0      -- 1 Ringgit Malaysia = Rp4.349
kursKeIDR THB = 544.0       -- 1 Baht Thailand = Rp544
kursKeIDR AUD = 12003.0     -- 1 Dollar Australia = Rp12.003
kursKeIDR CAD = 12349.0     -- 1 Dollar Kanada = Rp12.349
kursKeIDR CHF = 21861.0     -- 1 Franc Swiss = Rp21.861
kursKeIDR INR = 210.0       -- 1 Rupee India = Rp210 (estimasi dari kurs USD/INR)
kursKeIDR SAR = 4504.0      -- 1 Riyal Arab Saudi = Rp4.504

-- Konversi mata uang: ubah nilai dari mata uang 'asal' ke 'tujuan'
-- Menggunakan alur: Asal -> IDR -> Tujuan
convertCurrency :: MataUang -> MataUang -> Double -> Double
convertCurrency asal tujuan nilai =
    let nilaiDalamIDR = nilai * kursKeIDR asal
    in nilaiDalamIDR / kursKeIDR tujuan
