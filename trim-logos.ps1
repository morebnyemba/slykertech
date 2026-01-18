# Logo Optimization with ImageMagick
# Removes whitespace from top and bottom of logo images

$imageMagickPath = "C:\Program Files\ImageMagick-7.1.2-Q16-HDRI\magick.exe"
$logoDir = "public\images"
$logos = @("stws.png", "stws.jpeg", "stwsico.png")

Write-Host "Logo Optimization with ImageMagick" -ForegroundColor Cyan
Write-Host "===================================" -ForegroundColor Cyan
Write-Host ""

foreach ($logo in $logos) {
    $inputPath = Join-Path $logoDir $logo
    
    if (Test-Path $inputPath) {
        Write-Host "Processing: $logo" -ForegroundColor Yellow
        
        $baseName = [System.IO.Path]::GetFileNameWithoutExtension($logo)
        $extension = [System.IO.Path]::GetExtension($logo)
        $backupPath = Join-Path $logoDir "$($baseName)_original$($extension)"
        $tempPath = Join-Path $logoDir "$($baseName)_temp$($extension)"
        
        # Create backup
        Copy-Item -Path $inputPath -Destination $backupPath -Force
        Write-Host "  Backup: $($baseName)_original$($extension)" -ForegroundColor Gray
        
        # Trim whitespace using ImageMagick
        & "$imageMagickPath" "$inputPath" -trim -fuzz "10%" "$tempPath"
        
        if (Test-Path $tempPath) {
            Move-Item -Path $tempPath -Destination $inputPath -Force
            Write-Host "  Trimmed successfully" -ForegroundColor Green
        } else {
            Write-Host "  Trim failed" -ForegroundColor Red
        }
    }
    else {
        Write-Host "  File not found: $inputPath" -ForegroundColor Red
    }
    Write-Host ""
}

Write-Host "Complete!" -ForegroundColor Green
