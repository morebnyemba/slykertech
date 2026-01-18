# Create ICO files from PNG logos using ImageMagick

$imageMagickPath = "C:\Program Files\ImageMagick-7.1.2-Q16-HDRI\magick.exe"
$logoDir = "public\images"

Write-Host "Creating ICO files from PNG logos" -ForegroundColor Cyan
Write-Host "==================================" -ForegroundColor Cyan
Write-Host ""

# Create favicon from stws.png
$pngFile = Join-Path $logoDir "stws.png"
$icoFile = Join-Path $logoDir "stws.ico"

if (Test-Path $pngFile) {
    Write-Host "Creating: stws.ico from stws.png" -ForegroundColor Yellow
    
    # Create multiple sizes for the ICO file (16x16, 32x32, 64x64)
    & "$imageMagickPath" "$pngFile" -define icon:auto-resize="256,128,96,64,48,32,16" "$icoFile"
    
    if (Test-Path $icoFile) {
        Write-Host "  Created successfully: $icoFile" -ForegroundColor Green
    } else {
        Write-Host "  Failed to create ICO file" -ForegroundColor Red
    }
} else {
    Write-Host "  PNG file not found: $pngFile" -ForegroundColor Red
}

Write-Host ""

# Create smaller version for header icon
$headerIco = Join-Path $logoDir "header-icon.ico"
Write-Host "Creating: header-icon.ico (32x32)" -ForegroundColor Yellow

& "$imageMagickPath" "$pngFile" -resize "32x32" -define icon:auto-resize="32,24,16" "$headerIco"

if (Test-Path $headerIco) {
    Write-Host "  Created successfully: $headerIco" -ForegroundColor Green
} else {
    Write-Host "  Failed to create header icon" -ForegroundColor Red
}

Write-Host ""

# Also create stwsico.ico if stwsico.png exists
$icoSource = Join-Path $logoDir "stwsico.png"
$icoTarget = Join-Path $logoDir "stwsico-converted.ico"

if (Test-Path $icoSource) {
    Write-Host "Creating: stwsico-converted.ico from stwsico.png" -ForegroundColor Yellow
    & "$imageMagickPath" "$icoSource" -define icon:auto-resize="256,128,96,64,48,32,16" "$icoTarget"
    
    if (Test-Path $icoTarget) {
        Write-Host "  Created successfully: $icoTarget" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "ICO creation complete!" -ForegroundColor Green
