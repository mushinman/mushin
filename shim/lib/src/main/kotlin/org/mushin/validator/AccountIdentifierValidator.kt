package org.mushin.validator

import org.apache.commons.validator.routines.EmailValidator

/**
 *  A validator for ActivityPub federated accounts. Accepts any fully qualified account
 *  name that is also a valid email address. This is not the same thing as a valid
 *  nickname in mushin, which is more specific.
 */
public class AccountIdentifierValidator : EmailValidator(true, false) {
    /**
     *  Validates an account name. Any valid email username/account name
     *  is valid.
     *  @param accountName The account name to validate.
     *  @return True if the account name is valid, otherwise false.
     */
    public fun isValidAccountName(accountName: String): Boolean {
        return super.isValidUser(accountName)
    }

    /**
     *  Validates a domain name. Any valid email domain name is valid
     *  @param domain The domain name to validate.
     *  @return True if the domain name is valid, otherwise false.
     */
    public override fun isValidDomain(domain: String): Boolean {
        return super.isValidDomain(domain)
    }

    /**
     *  Validates a fully qualified account name (e.g. nickname@domain.org).
     *  @param acc A fully qualified account name to check.
     *  @return True if the account name is valid, otherwise false.
     */
    public fun isValidAccountIdentifier(acc: String): Boolean {
        // TODO maybe we should support @nickname@domain.org, or webfinger style
        // acc:nickname@domain.org?
        return super.isValid(acc)
    }
}
