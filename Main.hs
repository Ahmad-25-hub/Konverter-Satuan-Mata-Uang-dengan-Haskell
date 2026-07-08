module Main where

import System.IO (hFlush, stdout, hSetBuffering, BufferMode(LineBuffering))
import Text.Read (readMaybe)
import Converter

-- ============================================================================
-- HELPER FUNCTIONS FOR STRING AND INPUT VALIDATION
-- ============================================================================

-- Fungsi helper untuk menghapus whitespace (spasi, tab, newline) di awal dan akhir String
trim :: String -> String
trim = trimTail . trimHead
  where
    trimHead = dropWhile (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n')
    trimTail = reverse . trimHead . reverse

-- Fungsi untuk memformat tampilan Double agar lebih rapi.
-- Jika merupakan bilangan bulat, koma desimal (.0) akan dihilangkan.
-- Jika desimal, dibatasi maksimal 4 angka di belakang koma.
formatDouble :: Double -> String
formatDouble x
    | x == fromInteger (round x) = show (round x :: Integer)
    | otherwise                  = formatDec x
  where
    formatDec val =
        let s = show val
        in case break (== '.') s of
            (before, '.':after) -> before ++ "." ++ take 4 after
            _                   -> s

-- Validasi nilai fisik umum (tidak boleh negatif)
validasiNonNegatif :: Double -> Either String Double
validasiNonNegatif x
    | x < 0     = Left "Nilai tidak boleh negatif!"
    | otherwise = Right x

-- Validasi suhu khusus untuk Kelvin (tidak boleh di bawah 0 mutlak)
validasiSuhu :: SatuanSuhu -> Double -> Either String Double
validasiSuhu Kelvin x
    | x < 0     = Left "Suhu Kelvin tidak boleh di bawah 0 mutlak (0 K)!"
    | otherwise = Right x
validasiSuhu _ x = Right x


-- ============================================================================
-- INTERAKSI INPUT & DISPLAY MENU SATUAN
-- ============================================================================

-- Menampilkan daftar pilihan satuan beserta nomor indeksnya
tampilkanSatuan :: Show a => [a] -> IO ()
tampilkanSatuan list = mapM_ (\(i, item) -> putStrLn (show i ++ ". " ++ show item)) (zip [1..] list)

-- Membaca pilihan indeks satuan dari user secara aman
pilihSatuan :: Show a => [a] -> String -> IO (Maybe a)
pilihSatuan list prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    let trimmed = trim input
    if null trimmed
        then do
            putStrLn ">> Error: Input tidak boleh kosong!"
            return Nothing
        else case readMaybe trimmed of
            Just idx ->
                if idx >= 1 && idx <= length list
                    then return (Just (list !! (idx - 1)))
                    else do
                        putStrLn $ ">> Error: Pilihan tidak tersedia (1-" ++ show (length list) ++ ")!"
                        return Nothing
            Nothing -> do
                putStrLn ">> Error: Input harus berupa angka pilihan!"
                return Nothing

-- Meminta input nilai angka dari user beserta validasinya
mintaNilai :: String -> (Double -> Either String Double) -> IO (Maybe Double)
mintaNilai prompt validator = do
    putStr prompt
    hFlush stdout
    input <- getLine
    let trimmed = trim input
    if null trimmed
        then do
            putStrLn ">> Error: Input tidak boleh kosong!"
            return Nothing
        else case readMaybe trimmed of
            Just val ->
                case validator val of
                    Left errMsg -> do
                        putStrLn $ ">> Error: " ++ errMsg
                        return Nothing
                    Right cleanVal -> return (Just cleanVal)
            Nothing -> do
                putStrLn ">> Error: Input harus berupa angka!"
                return Nothing

-- Menampilkan hasil konversi dalam kotak format yang rapi
tampilkanHasil :: String -> String -> String -> String -> IO ()
tampilkanHasil nilaiAsal satuanAsal nilaiTujuan satuanTujuan = do
    putStrLn "\n====================================="
    putStrLn "            HASIL KONVERSI           "
    putStrLn "====================================="
    putStrLn $ nilaiAsal ++ " " ++ satuanAsal
    putStrLn "="
    putStrLn $ nilaiTujuan ++ " " ++ satuanTujuan
    putStrLn "=====================================\n"


-- ============================================================================
-- KATEGORI SUB-MENU KONVERSI
-- ============================================================================

-- 1. Menu Konversi Panjang
konversiPanjangMenu :: IO ()
konversiPanjangMenu = do
    putStrLn "\n====================================="
    putStrLn "          KONVERSI PANJANG           "
    putStrLn "====================================="
    tampilkanSatuan daftarPanjang
    
    mAsal <- pilihSatuan daftarPanjang "\nPilih satuan asal (1-8): "
    case mAsal of
        Nothing -> return () -- Kembali ke menu utama jika input asal tidak valid
        Just asal -> do
            mTujuan <- pilihSatuan daftarPanjang "Pilih satuan tujuan (1-8): "
            case mTujuan of
                Nothing -> return () -- Kembali ke menu utama jika input tujuan tidak valid
                Just tujuan -> do
                    mNilai <- mintaNilai "Masukkan nilai: " validasiNonNegatif
                    case mNilai of
                        Nothing -> return () -- Kembali ke menu utama jika nilai tidak valid
                        Just nilai -> do
                            let hasil = convertLength asal tujuan nilai
                            tampilkanHasil (formatDouble nilai) (show asal) (formatDouble hasil) (show tujuan)

-- 2. Menu Konversi Berat
konversiBeratMenu :: IO ()
konversiBeratMenu = do
    putStrLn "\n====================================="
    putStrLn "           KONVERSI BERAT            "
    putStrLn "====================================="
    tampilkanSatuan daftarBerat
    
    mAsal <- pilihSatuan daftarBerat "\nPilih satuan asal (1-6): "
    case mAsal of
        Nothing -> return ()
        Just asal -> do
            mTujuan <- pilihSatuan daftarBerat "Pilih satuan tujuan (1-6): "
            case mTujuan of
                Nothing -> return ()
                Just tujuan -> do
                    mNilai <- mintaNilai "Masukkan nilai: " validasiNonNegatif
                    case mNilai of
                        Nothing -> return ()
                        Just nilai -> do
                            let hasil = convertWeight asal tujuan nilai
                            tampilkanHasil (formatDouble nilai) (show asal) (formatDouble hasil) (show tujuan)

-- 3. Menu Konversi Suhu
konversiSuhuMenu :: IO ()
konversiSuhuMenu = do
    putStrLn "\n====================================="
    putStrLn "            KONVERSI SUHU            "
    putStrLn "====================================="
    tampilkanSatuan daftarSuhu
    
    mAsal <- pilihSatuan daftarSuhu "\nPilih satuan asal (1-3): "
    case mAsal of
        Nothing -> return ()
        Just asal -> do
            mTujuan <- pilihSatuan daftarSuhu "Pilih satuan tujuan (1-3): "
            case mTujuan of
                Nothing -> return ()
                Just tujuan -> do
                    -- Menggunakan validasiSuhu khusus untuk memeriksa batas mutlak Kelvin
                    mNilai <- mintaNilai "Masukkan nilai: " (validasiSuhu asal)
                    case mNilai of
                        Nothing -> return ()
                        Just nilai -> do
                            let hasil = convertTemperature asal tujuan nilai
                            tampilkanHasil (formatDouble nilai) (show asal) (formatDouble hasil) (show tujuan)

-- 4. Menu Konversi Waktu
konversiWaktuMenu :: IO ()
konversiWaktuMenu = do
    putStrLn "\n====================================="
    putStrLn "           KONVERSI WAKTU            "
    putStrLn "====================================="
    tampilkanSatuan daftarWaktu
    
    mAsal <- pilihSatuan daftarWaktu "\nPilih satuan asal (1-4): "
    case mAsal of
        Nothing -> return ()
        Just asal -> do
            mTujuan <- pilihSatuan daftarWaktu "Pilih satuan tujuan (1-4): "
            case mTujuan of
                Nothing -> return ()
                Just tujuan -> do
                    mNilai <- mintaNilai "Masukkan nilai: " validasiNonNegatif
                    case mNilai of
                        Nothing -> return ()
                        Just nilai -> do
                            let hasil = convertTime asal tujuan nilai
                            tampilkanHasil (formatDouble nilai) (show asal) (formatDouble hasil) (show tujuan)

-- 5. Menu Konversi Mata Uang
konversiMataUangMenu :: IO ()
konversiMataUangMenu = do
    putStrLn "\n====================================="
    putStrLn "         KONVERSI MATA UANG          "
    putStrLn "====================================="
    tampilkanSatuan daftarMataUang
    
    mAsal <- pilihSatuan daftarMataUang "\nPilih mata uang asal (1-15): "
    case mAsal of
        Nothing -> return ()
        Just asal -> do
            mTujuan <- pilihSatuan daftarMataUang "Pilih mata uang tujuan (1-15): "
            case mTujuan of
                Nothing -> return ()
                Just tujuan -> do
                    mNilai <- mintaNilai "Masukkan nilai uang: " validasiNonNegatif
                    case mNilai of
                        Nothing -> return ()
                        Just nilai -> do
                            let hasil = convertCurrency asal tujuan nilai
                            tampilkanHasil (formatDouble nilai) (show asal) (formatDouble hasil) (show tujuan)


-- ============================================================================
-- MAIN LOOP DENGAN REKURSI
-- ============================================================================

-- Menu utama program
mainMenu :: IO ()
mainMenu = do
    putStrLn "====================================="
    putStrLn "    KONVERTER SATUAN & MATA UANG     "
    putStrLn "====================================="
    putStrLn "1. Konversi Panjang"
    putStrLn "2. Konversi Berat"
    putStrLn "3. Konversi Suhu"
    putStrLn "4. Konversi Waktu"
    putStrLn "5. Konversi Mata Uang"
    putStrLn "0. Keluar"
    putStrLn "====================================="
    putStr "Pilih menu : "
    hFlush stdout
    pilihan <- getLine
    let trimmed = trim pilihan
    case trimmed of
        "1" -> konversiPanjangMenu >> mainMenu
        "2" -> konversiBeratMenu >> mainMenu
        "3" -> konversiSuhuMenu >> mainMenu
        "4" -> konversiWaktuMenu >> mainMenu
        "5" -> konversiMataUangMenu >> mainMenu
        "0" -> putStrLn "\nTerima kasih telah menggunakan program ini. Sampai jumpa!"
        _   -> do
            putStrLn "\nPilihan tidak tersedia."
            putStrLn "Silakan coba lagi.\n"
            mainMenu

-- Fungsi utama (Entry Point)
main :: IO ()
main = do
    -- Set buffer stdout ke LineBuffering agar output segera tercetak ke terminal
    hSetBuffering stdout LineBuffering
    mainMenu

