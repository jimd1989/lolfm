/* Makes lolfm a good pipe citizen.
 * See https://stackoverflow.com/questions/65755853 */

use libc;

#[cfg(unix)]
pub fn reset_sigpipe() {
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }
}

#[cfg(not(unix))]
pub fn reset_sigpipe() {}
