package annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ValueHintIterator {
    int start() default 0;
    int restart() default 0;
    int step() default 0;
    int initial() default 0;
}
