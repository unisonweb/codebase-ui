const macosPlatforms = ["Macintosh", "MacIntel", "MacPPC", "Mac68K"];
const windowsPlatforms = ["Win32", "Win64", "Windows", "WinCE"];
const iosPlatforms = ["iPhone", "iPad", "iPod"];

function detectOs(nav) {
  const { userAgent, platform } = nav;

  if (macosPlatforms.includes(platform)) {
    return "macOS";
  } else if (iosPlatforms.includes(platform)) {
    return "iOS";
  } else if (windowsPlatforms.includes(platform)) {
    return "Windows";
  } else if (/Android/.test(userAgent)) {
    return "Android";
  } else if (/Linux/.test(platform)) {
    return "Linux";
  } else {
    return "Unknown";
  }
}

function platform() {
  return {
    operatingSystem: detectOs(window.navigator),
  };
}

export default platform;
